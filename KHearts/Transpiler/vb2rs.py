#!/usr/bin/env python3
"""Translates Killer Hearts' VB6 engine to Rust, as vb2cs.py does to C#, with the same parser, analysis
and source patches.

    python3 vb2rs.py <KHearts source directory> <output directory>

The engine's state (VB's globals, module variables, static locals and form controls) becomes one struct,
KhEngine, and each module's procedures become its methods. VB's semantics carry over as in vb2cs.py, with
these differences for Rust:

- A ByRef argument is copied in and back out, in parameter order. No call in the engine passes a variable
  that its callee also reaches directly, so this matches VB's aliasing.
- diagnose_and_halt, End, a bad array index and division by zero set a halt flag instead of aborting:
  each statement that calls a procedure then returns, and each loop stops, so the caller can recover.
- Calls to the form's user interface (billboard, sounds, messages) are dropped, with their arguments,
  unless an argument calls a procedure.
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import vb2cs as vb                                  # noqa: E402
from vb2cs import Proc, vb_type, SOURCES            # noqa: E402

RUST_KEYWORDS = set('''as break const continue crate else enum extern false fn for if impl in let loop match mod
move mut pub ref return self Self static struct super trait true type unsafe use where while async await dyn
abstract become box do final macro override priv typeof unsized virtual yield try gen union'''.split())

RS_TYPES = {'int': 'i32', 'bool': 'bool', 'float': 'f32', 'double': 'f64', 'string': 'String'}

DEFAULTS = {'int': '0', 'bool': 'false', 'float': '0.0f32', 'double': '0.0f64', 'string': 'String::new()'}

# The user interface: calls are dropped (see above)
UI_PROCS = {'bb_add', 'bb_cla', 'notice_board_cla', 'display_center_message', 'capture_snapshot', 'print_instantaneous_data',
            'playsoundx', 'turn_off_sound'}

ALLOWS = ('non_snake_case, non_upper_case_globals, non_camel_case_types, unused_variables, unused_mut, '
          'unused_assignments, unused_labels, unused_parens, unused_imports, unreachable_code, dead_code, '
          'clippy::all')


# The arena's rules (std-1) differ from Killer Hearts': the Q♠ doesn't break hearts, so can_lead_H can be
# false after QS_played. The first two lines lead a heart whenever the Q♠ has been played, assuming hearts are
# then broken; the guard keeps them to Killer Hearts' play under its own rules, where QS_played implies
# can_lead_H. The third is where a leader claims the remaining tricks after the Q♠ with the top hearts, but has
# no suit it may lead to start: Killer Hearts halts, as under its own rules the leader always has one (hearts,
# once the Q♠ is played). For the arena, the leader claims anyway, as through V7.03, and plays on with its
# highest legal card (see table.rs). Not claiming instead leads to other lines that lead a heart.
ARENA_PATCHES = [
    ('Playing.bas',
     'If est_tr(hearts) > 0 And LU(hearts) >= mini(est_tr(hearts), 2) Then',
     "If est_tr(hearts) > 0 And LU(hearts) >= mini(est_tr(hearts), 2) And can_lead_H Then 'patched: see ARENA_PATCHES"),
    ('Playing.bas',
     'ElseIf pts_sit = player And HU(hearts) < 2 And suit_len_H > HU(hearts) And mode <> Equivocate Then',
     'ElseIf pts_sit = player And HU(hearts) < 2 And suit_len_H > HU(hearts) And mode <> Equivocate And can_lead_H '
     "Then 'patched: see ARENA_PATCHES"),
    ('Playing.bas',
     'If suit_with_HU = illegal_value Then Call diagnose_and_halt("No suit with HU")',
     "'If suit_with_HU = illegal_value Then Call diagnose_and_halt(\"No suit with HU\") patched: see ARENA_PATCHES"),
]


def rs_name(cs_name):
    n = cs_name.lstrip('@')
    return n + '_' if n in RUST_KEYWORDS else n


def rs_type(t):
    if isinstance(t, tuple):
        return f'VbArray<{RS_TYPES[t[1]]}>'
    return RS_TYPES[t]


def paren(code):
    return code if code.startswith('(') and code.endswith(')') else f'({code})'


class E:
    """A Rust expression: its code, VB type, whether it calls a procedure, and, for a string, whether it
    is a literal ('lit'), a place to borrow ('place') or a new String ('owned')."""

    def __init__(self, code, t, call=False, skind='owned'):
        self.code, self.t, self.call, self.skind = code, t, call, skind


def as_str(e):
    """A string expression as &str."""
    if e.skind in ('lit', 'ref'):
        return e.code
    return f'&{paren(e.code)}' if e.skind == 'owned' else f'&{e.code}'


def as_string(e):
    """A string expression as a String."""
    if e.skind in ('lit', 'ref'):
        return f'String::from({e.code})'
    if e.skind == 'place':
        return f'{e.code}.clone()'
    return e.code


def conv(e, to):
    """Converts an expression to type `to`, VB6-style, answering code."""
    frm, c = e.t, e.code
    if to is None or isinstance(to, tuple) or isinstance(frm, tuple):
        return c
    if frm == to:
        return as_string(e) if to == 'string' else c
    if frm == 'string':
        s = as_str(e)
        return {'int': f'vb::clng(vb::val({s}))', 'bool': f'vb::cbool({s})', 'float': f'(vb::val({s}) as f32)',
                'double': f'vb::val({s})'}[to]
    if to == 'int':
        return f'vb::b2i({c})' if frm == 'bool' else f'vb::clng({c} as f64)'
    if to == 'bool':
        return f'({c} != 0)' if frm == 'int' else f'({c} != 0.0)'
    if to == 'float':
        return f'(vb::b2i({c}) as f32)' if frm == 'bool' else f'({c} as f32)'
    if to == 'double':
        return f'(vb::b2i({c}) as f64)' if frm == 'bool' else f'({c} as f64)'
    if to == 'string':
        return {'int': f'vb::cstr_i({c})', 'bool': f'vb::cstr_b({c})', 'float': f'vb::cstr_f({c} as f64)',
                'double': f'vb::cstr_f({c})'}[frm]
    raise ValueError(f'cannot convert {frm} to {to}')


class RsGen:
    """Generates Rust for one procedure (or, with proc None, for module-level expressions)."""

    def __init__(self, tr, proc, consts):
        self.tr, self.proc, self.consts = tr, proc, consts
        self.out = []
        self.indent = 2
        self.loops = []                     # stack of (kind, label, continue label)
        self.local_names = {}

    # ---- names

    def var_code(self, v):
        n = rs_name(v.cs_name)
        if v.kind in ('global', 'module', 'static'):
            return f'self.{n}'
        if v.kind == 'const':
            return n
        if v.kind == 'ret':
            return '__ret'
        n = self.local_names.get(v.name.lower(), n)
        if v.kind == 'param' and self.proc.ref_params[v.param_index]:
            return f'(*{n})'
        return n

    def lookup(self, name):
        if self.proc is None:
            low = name.lower()
            return self.tr.globals.get(low) or self.module_lookup(low)
        return self.tr.lookup_var(self.proc, name)

    def module_lookup(self, low):
        for (_, l), v in self.tr.module_vars.items():
            if l == low and v.kind == 'const':
                return v
        return None

    def emit(self, line, comment=None):
        text = '    ' * self.indent + line
        if comment:
            text += ('  ' if line else '') + '// ' + comment
        self.out.append(text.rstrip())

    def tmp(self, prefix='__t'):
        self.tr.temp += 1
        return f'{prefix}{self.tr.temp}'

    # ---- expressions

    def expr(self, e, want=None):
        x = self.ex(e)
        return conv(x, want) if want else (as_string(x) if x.t == 'string' else x.code)

    def ex(self, e):
        k = e.kind
        if k == 'num':
            if e.t == 'int':
                return E(str(e.v) if e.v >= 0 else f'({e.v})', 'int')
            v = repr(float(e.v))
            if 'e' not in v and '.' not in v:
                v += '.0'
            suffix = 'f32' if e.t == 'float' else 'f64'
            return E(f'{v}{suffix}' if float(e.v) >= 0 else f'({v}{suffix})', e.t)
        if k == 'str':
            return E('"' + e.v.replace('\\', '\\\\').replace('"', '\\"') + '"', 'string', skind='lit')
        if k == 'paren':
            x = self.ex(e.e)
            if x.t == 'string':
                return E(as_string(x), 'string', x.call)
            return E(paren(x.code), x.t, x.call)
        if k == 'name':
            return self.ex_name(e.name)
        if k == 'index':
            return self.ex_index(e)
        if k == 'member':
            return self.ex_member(e)
        if k == 'unary':
            x = self.ex(e.e)
            if e.op == '-':
                if x.t == 'bool':
                    return E(f'(-vb::b2i({x.code}))', 'int', x.call)
                return E(f'(-{paren(x.code)})', x.t, x.call)
            if x.t == 'bool':
                return E(f'(!{paren(x.code)})', 'bool', x.call)
            return E(f'(!{paren(conv(x, "int"))})', 'int', x.call)
        if k == 'bin':
            return self.ex_bin(e)
        raise ValueError(f'unknown expression {e}')

    def ex_name(self, name):
        low = name.lower()
        v = self.lookup(name)
        if v is not None:
            if v.is_array:
                return E(self.var_code(v), v.type, skind='place')
            if v.kind == 'const':
                return E(rs_name(v.cs_name), v.ctype, skind='lit')
            if v.kind == 'param' and v.ctype == 'string':
                return E(self.var_code(v), v.ctype, skind='ref')
            return E(self.var_code(v), v.ctype, skind='place')
        if low in vb.BUILTIN_CONSTS:
            code, t = vb.BUILTIN_CONSTS[low]
            return E(code, t, skind='lit')
        callee = self.tr.find_proc(self.proc.module, name)
        if callee is not None:
            return self.call(callee, name, [])
        raise ValueError(f'{self.proc.name}: unknown name {name}')

    def indices(self, args):
        return [self.ex(a) for a in args]

    def ex_index(self, e):
        v = self.lookup(e.name)
        if v is not None and v.is_array:
            idx = self.indices(e.args)
            base = self.var_code(v)
            k = len(idx)
            skind = 'owned'
            if any(i.call for i in idx):
                names = [self.tmp('__i') for _ in idx]
                lets = ' '.join(f'let {n}: i32 = {conv(i, "int")};' for n, i in zip(names, idx))
                return E(f'{{ {lets} {base}.get{k}({", ".join(names)}) }}', v.ctype, True, skind)
            return E(f'{base}.get{k}({", ".join(conv(i, "int") for i in idx)})', v.ctype, False, skind)
        low = e.name.lower()
        if low in ('csng', 'cdbl', 'clng', 'cint', 'cbool', 'cstr'):
            target = {'csng': 'float', 'cdbl': 'double', 'clng': 'int', 'cint': 'int', 'cbool': 'bool',
                      'cstr': 'string'}[low]
            x = self.ex(e.args[0])
            return E(conv(x, target), target, x.call)
        if low in ('int', 'fix'):
            x = self.ex(e.args[0])
            return E(f'vb::{low}_({conv(x, "double")})', 'double', x.call)
        if low == 'abs':
            x = self.ex(e.args[0])
            return E(f'{paren(x.code)}.abs()', x.t, x.call)
        if low == 'len':
            x = self.ex(e.args[0])
            return E(f'vb::len({as_str(x)})', 'int', x.call)
        callee = self.tr.find_proc(self.proc.module, e.name)
        if callee is not None:
            return self.call(callee, e.name, e.args)
        raise ValueError(f'{self.proc.name}: unknown function or array {e.name}')

    def ex_member(self, e):
        root = e
        parts = []
        while root.kind == 'member':
            parts.append(root.name)
            root = root.obj
        parts.append(root.name)
        parts.reverse()
        if parts[0].lower() == 'red_felt':
            parts = parts[1:]
        field = 'ui_' + '_'.join(parts)
        prop = parts[-1].lower()
        t = 'bool' if prop in ('checked', 'visible', 'enabled', 'value') else (
            'string' if prop in ('caption', 'text') else 'int')
        self.tr.ui_fields[field] = t
        return E(f'self.{field}', t, skind='place')

    def ex_bin(self, e):
        op = e.op
        a, b = self.ex(e.a), self.ex(e.b)
        call = a.call or b.call
        if op in ('and', 'or', 'xor'):
            sym = {'and': '&', 'or': '|', 'xor': '^'}[op]
            if a.t == 'bool' and b.t == 'bool':
                return E(f'({a.code} {sym} {b.code})', 'bool', call)
            return E(f'({conv(a, "int")} {sym} {conv(b, "int")})', 'int', call)
        if op in ('=', '<>', '<', '>', '<=', '>='):
            sym = {'=': '==', '<>': '!='}.get(op, op)
            if a.t == 'string' or b.t == 'string':
                sa = as_str(a) if a.t == 'string' else f'&{conv(a, "string")}'
                sb = as_str(b) if b.t == 'string' else f'&{conv(b, "string")}'
                if op in ('=', '<>'):
                    return E(f'({sa} {sym} {sb})', 'bool', call)
                return E(f'(vb::str_cmp({sa}, {sb}) {op} 0)', 'bool', call)
            if a.t == 'bool' and b.t == 'bool' and op in ('=', '<>'):
                return E(f'({a.code} {sym} {b.code})', 'bool', call)
            t = self.numeric(a.t, b.t)
            return E(f'({conv(a, t)} {sym} {conv(b, t)})', 'bool', call)
        if op == '&' or (op == '+' and (a.t == 'string' or b.t == 'string')):
            sa = as_str(a) if a.t == 'string' else f'&{conv(a, "string")}'
            sb = as_str(b) if b.t == 'string' else f'&{conv(b, "string")}'
            return E(f'vb::cat({sa}, {sb})', 'string', call)
        if op == '/':
            return E(f'({conv(a, "double")} / {conv(b, "double")})', 'double', call)
        if op == '\\':
            return E(f'vb::idiv({conv(a, "int")}, {conv(b, "int")})', 'int', call)
        if op == 'mod':
            return E(f'vb::imod({conv(a, "int")}, {conv(b, "int")})', 'int', call)
        if op == '^':
            return E(f'{paren(conv(a, "double"))}.powf({conv(b, "double")})', 'double', call)
        if op in ('+', '-', '*'):
            t = self.numeric(a.t, b.t)
            return E(f'({conv(a, t)} {op} {conv(b, t)})', t, call)
        raise ValueError(f'unsupported operator {op}')

    def numeric(self, ta, tb):
        r = max(vb.NUMERIC_RANK.get(ta, 0), vb.NUMERIC_RANK.get(tb, 0))
        return ['int', 'float', 'double'][r]

    # ---- calls

    def place(self, a):
        """For a ByRef argument: (lets, read, write template) if it is a variable or array element, else None."""
        if a.kind == 'name':
            v = self.lookup(a.name)
            if v is not None and not v.is_array and v.kind != 'const':
                code = self.var_code(v)
                return [], code, code + ' = {};', v.ctype
        if a.kind == 'index':
            v = self.lookup(a.name)
            if v is not None and v.is_array:
                idx = self.indices(a.args)
                names = [self.tmp('__i') for _ in idx]
                lets = [f'let {n}: i32 = {conv(i, "int")};' for n, i in zip(names, idx)]
                base = self.var_code(v)
                k = len(idx)
                return (lets, f'{base}.get{k}({", ".join(names)})',
                        f'{base}.set{k}({", ".join(names)}, {{}});', v.ctype)
        return None

    def call(self, callee, name, args):
        low = name.lower()
        if isinstance(callee, Proc):
            if callee.excluded:
                self.tr.warn(f'{self.proc.name} calls excluded {callee.name}')
                rt = callee.rtype or None
                code = f'vb::not_ported::<{rs_type(rt)}>("{callee.name}")' if rt else f'vb::not_ported::<()>("{callee.name}")'
                return E(code, rt, True)
            ptypes = [vb_type(p.vtype) for p in callee.params]
            refs = callee.ref_params
            fn = f'self.{rs_name(callee.cs_name)}'
            rtype = callee.rtype
        else:
            return self.handwritten(low, args)
        return self.call_with(fn, ptypes, refs, args, rtype, method=True)

    def call_with(self, fn, ptypes, refs, args, rtype, method):
        """A call, with arguments evaluated in order into temporaries when any of them calls a procedure or
        is passed ByRef."""
        xs = []
        needs_block = False
        for j, a in enumerate(args):
            pt = ptypes[j]
            if refs[j]:
                xs.append(('ref', a, pt))
                needs_block = True
            else:
                x = self.ex(a)
                xs.append(('val', x, pt))
                if x.call:
                    needs_block = True
                if pt == 'string' and x.skind == 'place' and method:
                    needs_block = True          # a borrowed field would conflict with &mut self
        if not needs_block:
            parts = []
            for _, x, pt in xs:
                parts.append(as_str(x) if pt == 'string' else conv(x, pt))
            return E(f'{fn}({", ".join(parts)})', rtype, method, 'owned')
        lets, parts, writes = [], [], []
        for kind, x, pt in xs:
            n = self.tmp('__a')
            if kind == 'ref':
                p = self.place(x)
                if p is None:
                    lets.append(f'let mut {n}: {rs_type(pt)} = {self.expr(x, pt)};')
                else:
                    plets, read, write, vt = p
                    lets.extend(plets)
                    lets.append(f'let mut {n}: {rs_type(pt)} = {read};')
                    writes.append(write.format(n))
                parts.append(f'&mut {n}')
            elif pt == 'string':
                if x.skind in ('lit', 'ref'):
                    parts.append(x.code)
                else:
                    lets.append(f'let {n}: String = {as_string(x)};')
                    parts.append(f'&{n}')
            else:
                lets.append(f'let {n}: {rs_type(pt)} = {conv(x, pt)};')
                parts.append(n)
        body = ' '.join(lets)
        res = f'{fn}({", ".join(parts)})'
        if writes:
            r = self.tmp('__r')
            return E(f'{{ {body} let {r} = {res}; {" ".join(writes)} {r} }}', rtype, True, 'owned')
        return E(f'{{ {body} {res} }}', rtype, True, 'owned')

    def handwritten(self, low, args):
        xs = lambda: [self.ex(a) for a in args]
        if low in ('get_next_player', 'get_previous_player', 'count_bits_on'):
            x = self.ex(args[0])
            return E(f'{low}({conv(x, "int")})', 'int', x.call)
        if low in ('mini', 'maxi', 'minr', 'maxr'):
            ps = xs()
            rt = 'int' if low in ('mini', 'maxi') else 'float'
            return E(f'{low}(&[{", ".join(conv(x, "double") for x in ps)}])', rt, any(x.call for x in ps))
        if low == 'truth_map':
            p, t, f = xs()
            return E(f'truth_map({conv(p, "bool")}, {conv(t, "int")}, {conv(f, "int")})', 'int',
                     p.call or t.call or f.call)
        if low == 'bit_numbers_on':
            x = self.ex(args[0])
            return E(f'bit_numbers_on({conv(x, "int")})', 'string', x.call)
        if low == 'format_i':
            a, w = xs()
            return E(f'format_I({conv(a, "int")}, {conv(w, "int")})', 'string', a.call or w.call)
        if low == 'format_f':
            a, w, d = xs()
            return E(f'format_F({conv(a, "float")}, {conv(w, "int")}, {conv(d, "int")})', 'string',
                     a.call or w.call or d.call)
        if low == 'name_string':
            v = self.lookup(args[0].name)
            return E(f'name_string(&{self.var_code(v)}, self.player, &self.player_name)', 'string')
        if low == 'indirect_sort':
            a, b = (self.lookup(x.name) for x in args)
            return E(f'indirect_sort(&{self.var_code(a)}, &mut {self.var_code(b)})', None)
        if low == 'get_index_from_rank_and_suit':
            return self.call_with('get_index_from_rank_and_suit', ['int'] * 3, [True, False, False], args, None,
                                  method=False)
        if low == 'get_rank_and_suit_from_index':
            return self.call_with('get_rank_and_suit_from_index', ['int'] * 3, [False, True, True], args, None,
                                  method=False)
        if low == 'diagnose_and_halt':
            x = self.ex(args[0])
            return E(f'vb::halt({as_str(x)})', None, True)
        if low in UI_PROCS:
            ps = xs()
            if any(x.call for x in ps):
                return E('{ ' + ' '.join(f'let _ = {self.expr_of(x)};' for x in ps) + ' }', None, True)
            return E('', None, False)
        raise ValueError(f'{self.proc.name}: unknown hand-written procedure {low}')

    def expr_of(self, x):
        return as_string(x) if x.t == 'string' else x.code

    # ---- statements

    def halted_return(self):
        return 'return __ret;' if self.proc.is_function else 'return;'

    def check_halt(self):
        self.emit(f'if vb::halted() {{ {self.halted_return()} }}')

    def stmts(self, ss):
        for s in ss:
            self.stmt(s)

    def stmt(self, s):
        k = s.kind
        c = getattr(s, 'comment', None)
        if k == 'comment':
            self.emit('', s.text)
        elif k == 'dim':
            if c:
                self.emit('', c)
        elif k == 'assign':
            self.assign(s, c)
        elif k == 'call':
            callee = self.tr.find_proc(self.proc.module, s.name)
            if callee is None:
                raise ValueError(f'{self.proc.name}: unknown procedure {s.name}')
            x = self.call(callee, s.name, s.args)
            if not x.code:
                self.emit('', f'(UI) {s.name}' + (f'  {c}' if c else ''))
                return
            self.emit(x.code + ';', c)
            if x.call:
                self.check_halt()
        elif k == 'exit':
            self.exit(s.what, c)
        elif k == 'erase':
            for n in s.names:
                v = self.lookup(n)
                if v is None or not v.is_array:
                    raise ValueError(f'{self.proc.name}: cannot Erase {n}')
                self.emit(f'{self.var_code(v)}.clear();', c)
                c = None
        elif k == 'if':
            self.if_block(s)
        elif k == 'ifline':
            self.emit(f'if {self.cond(s.cond)} {{', c)
            self.block(s.then)
            if s.els:
                self.emit('} else {')
                self.block(s.els)
            self.emit('}')
        elif k == 'selectcase':
            self.select(s)
        elif k == 'for':
            self.for_loop(s)
        elif k == 'do':
            self.do_loop(s)
        elif k == 'nop':
            self.emit('', f'(not ported) {s.text}' + (f'  {c}' if c else ''))
        elif k == 'ui':
            self.emit('', f'(UI) {s.text}' + (f'  {c}' if c else ''))
        elif k == 'goto':
            if s.label.lower() == 'l1' and self.proc.name == 'initialize_general_parameters':
                label = next(cl for kind, _, cl in reversed(self.loops) if kind == 'for')
                self.emit(f"break '{label};", 'GoTo L1' + (f'  {c}' if c else ''))
            else:
                raise ValueError(f'{self.proc.name}: unsupported GoTo {s.label}')
        elif k == 'label':
            self.emit('', f'{s.name}:')
        elif k == 'end':
            self.emit('vb::halt("End");', c)
            self.emit(self.halted_return())
        else:
            raise ValueError(f'{self.proc.name}: unsupported statement {k}')

    def block(self, ss):
        self.indent += 1
        self.stmts(ss)
        self.indent -= 1

    def cond(self, e):
        return conv(self.ex(e), 'bool')

    def assign(self, s, c):
        lhs = s.lhs
        if lhs.kind == 'member':
            target = self.ex_member(lhs)
            x = self.ex(s.rhs)
            self.emit(f'{target.code} = {conv(x, target.t)};', c)
            return
        if lhs.kind == 'index' and lhs.name.lower() == 'mid':
            self.emit('', '(not ported) Mid statement' + (f'  {c}' if c else ''))
            return
        v = self.lookup(lhs.name)
        if v is None:
            raise ValueError(f'{self.proc.name}: assignment to unknown {lhs.name}')
        if lhs.kind == 'name':
            if v.is_array:
                raise ValueError(f'{self.proc.name}: assignment to array {lhs.name}')
            x = self.ex(s.rhs)
            self.emit(f'{self.var_code(v)} = {conv(x, v.ctype)};', c)
            if x.call:
                self.check_halt()
            return
        if lhs.kind == 'index':
            if not v.is_array:
                raise ValueError(f'{self.proc.name}: assignment to unknown array {lhs.name}')
            idx = self.indices(lhs.args)
            x = self.ex(s.rhs)
            base = self.var_code(v)
            k = len(idx)
            if any(i.call for i in idx) or x.call:
                names = [self.tmp('__i') for _ in idx]
                lets = ' '.join(f'let {n}: i32 = {conv(i, "int")};' for n, i in zip(names, idx))
                val = self.tmp('__v')
                self.emit(f'{{ {lets} let {val}: {RS_TYPES[v.ctype]} = {conv(x, v.ctype)}; '
                          f'{base}.set{k}({", ".join(names)}, {val}); }}', c)
                self.check_halt()
            else:
                self.emit(f'{base}.set{k}({", ".join(conv(i, "int") for i in idx)}, {conv(x, v.ctype)});', c)
            return
        raise ValueError(f'{self.proc.name}: bad assignment target')

    def exit(self, what, c):
        if what in ('sub', 'function'):
            self.emit('return __ret;' if self.proc.is_function else 'return;', c)
            return
        for kind, label, _ in reversed(self.loops):
            if kind == what:
                self.emit(f"break '{label};", c)
                return
        raise ValueError(f'{self.proc.name}: Exit {what} outside a loop')

    def if_block(self, s):
        comments = [s.comment] + list(getattr(s, 'clause_comments', []))
        for i, (cond, body) in enumerate(s.clauses):
            head = 'if' if i == 0 else '} else if'
            self.emit(f'{head} {self.cond(cond)} {{', comments[i])
            self.block(body)
        if s.els is not None:
            self.emit('} else {', getattr(s, 'else_comment', None))
            self.block(s.els)
        self.emit('}')

    def select(self, s):
        t = self.tmp()
        x = self.ex(s.e)
        ty = x.t
        self.emit('{', s.comment)
        self.indent += 1
        self.emit(f'let {t}: {rs_type(ty)} = {self.expr_of(x)};')
        tx = E(t, ty, skind='place')
        first = True
        for items, body, comment in s.cases:
            conds = []
            for it in items:
                if it[0] == 'eq':
                    y = self.ex(it[1])
                    if ty == 'string':
                        conds.append(f'({as_str(tx)} == {as_str(y)})')
                    else:
                        tt = self.numeric(ty, y.t)
                        conds.append(f'({conv(tx, tt)} == {conv(y, tt)})')
                elif it[0] == 'range':
                    conds.append(f'({t} >= {self.expr(it[1], ty)} && {t} <= {self.expr(it[2], ty)})')
                else:
                    sym = {'=': '==', '<>': '!='}.get(it[1], it[1])
                    conds.append(f'({t} {sym} {self.expr(it[2], ty)})')
            self.emit(('if' if first else '} else if') + f' {" || ".join(conds)} {{', comment)
            self.block(body)
            first = False
        if s.els is not None:
            if first:
                self.emit('{')
            else:
                self.emit('} else {', getattr(s, 'else_comment', None))
            self.block(s.els)
        if not first or s.els is not None:
            self.emit('}')
        self.indent -= 1
        self.emit('}')

    def loop_var(self, var):
        if var.kind != 'name':
            raise ValueError(f'{self.proc.name}: unsupported For variable')
        v = self.lookup(var.name)
        if v is None:
            raise ValueError(f'{self.proc.name}: unknown For variable {var.name}')
        return self.var_code(v), v.ctype

    def for_loop(self, s):
        name, t = self.loop_var(s.var)
        rt = RS_TYPES[t]
        end = self.expr(s.end, t)
        start = self.expr(s.start, t)
        e = self.tmp('__e')
        label, cont = self.tmp('__l').lstrip('_'), self.tmp('__c').lstrip('_')
        one = '1' if t == 'int' else ('1.0f32' if t == 'float' else '1.0f64')
        self.emit('{', s.comment)
        self.indent += 1
        self.emit(f'let {e}: {rt} = {end};')
        if s.step is None:
            step, sign = one, 1
        elif s.step.kind == 'num' or (s.step.kind == 'unary' and s.step.e.kind == 'num'):
            val = s.step.v if s.step.kind == 'num' else -s.step.e.v
            sign = 1 if val >= 0 else -1
            step = self.expr(s.step, t)
        else:
            step, sign = self.tmp('__s'), 0
            self.emit(f'let {step}: {rt} = {self.expr(s.step, t)};')
        self.emit(f'{name} = {start};')
        if sign > 0:
            test = f'{name} <= {e}'
        elif sign < 0:
            test = f'{name} >= {e}'
        else:
            zero = '0' if t == 'int' else '0.0'
            test = f'(if {step} >= {zero} {{ {name} <= {e} }} else {{ {name} >= {e} }})'
        self.emit(f"'{label}: while {test} && !vb::halted() {{")
        self.indent += 1
        self.emit(f"'{cont}: {{")
        self.loops.append(('for', label, cont))
        self.block(s.body)
        self.loops.pop()
        self.emit('}')
        self.emit(f'{name} = {name} + {paren(step)};')
        self.indent -= 1
        self.emit('}')
        self.indent -= 1
        self.emit('}')

    def do_loop(self, s):
        label = self.tmp('__l').lstrip('_')
        self.loops.append(('do', label, None))
        if s.how is not None:
            cond = self.cond(s.cond)
            if s.how == 'until':
                cond = f'!{paren(cond)}'
            self.emit(f"'{label}: while {cond} && !vb::halted() {{", s.comment)
            self.block(s.body)
            self.emit('}')
        else:
            self.emit(f"'{label}: loop {{", s.comment)
            self.block(s.body)
            self.indent += 1
            if s.post_how is not None:
                cond = self.cond(s.post_cond)
                if s.post_how == 'while':
                    cond = f'!{paren(cond)}'
                self.emit(f'if {cond} || vb::halted() {{ break; }}')
            else:
                self.emit('if vb::halted() { break; }')
            self.indent -= 1
            self.emit('}')
        self.loops.pop()

    # ---- procedure

    def generate(self, fields):
        proc = self.proc
        node = proc.node
        out = []
        for lc in node.lead_comments:
            out.append(f'    // {lc}')
        params = []
        for i, p in enumerate(node.params):
            n = rs_name(vb.cs_ident(p.name))
            if n in self.consts:
                n += '_'
            self.local_names[p.name.lower()] = n
            t = vb_type(p.vtype)
            if p.is_array or p.paramarray:
                raise ValueError(f'{proc.name}: array parameters are not supported')
            if proc.ref_params[i]:
                params.append(f'{n}: &mut {RS_TYPES[t]}')
            elif t == 'string':
                params.append(f'{n}: &str')
            else:
                params.append(f'mut {n}: {RS_TYPES[t]}')
        ret = f' -> {RS_TYPES[proc.rtype]}' if proc.is_function else ''
        sig = f'    pub(crate) fn {rs_name(proc.cs_name)}(&mut self{"".join(", " + p for p in params)}){ret} {{'
        if node.comment:
            sig += f'  // {node.comment}'
        out.append(sig)
        self.indent = 2
        for v in proc.locals.values():
            if v.kind == 'const':
                fields.consts.append(f'pub const {rs_name(v.cs_name)}: {const_type(v.ctype)} = '
                                     f'{RsGen(self.tr, None, self.consts).const_expr(v.const_value, v.ctype)};')
            elif v.kind == 'static':
                fields.fields.append(field_decl(self.tr, v, self.consts, proc))
            elif v.kind == 'local':
                n = rs_name(v.cs_name)
                if n in self.consts:
                    n += '_'
                self.local_names[v.name.lower()] = n
                if v.is_array:
                    self.emit(f'let mut {n}: {rs_type(v.type)} = VbArray::new(&[{bounds(self.tr, v, self.consts, proc)}]);')
                else:
                    self.emit(f'let mut {n}: {RS_TYPES[v.ctype]} = {DEFAULTS[v.ctype]};')
        if proc.is_function:
            self.emit(f'let mut __ret: {RS_TYPES[proc.rtype]} = {DEFAULTS[proc.rtype]};')
        self.stmts(node.body)
        if proc.is_function:
            self.emit('__ret')
        out.extend(self.out)
        out.append('    }')
        return out

    def const_expr(self, e, t):
        x = self.ex(e)
        if t == 'string':
            return x.code
        return conv(x, t)


def const_type(t):
    return '&str' if t == 'string' else RS_TYPES[t]


def bounds(tr, v, consts, proc=None):
    gen = RsGen(tr, proc, consts)
    return ', '.join(f'{gen.expr(lo, "int")}, {gen.expr(hi, "int")}' for lo, hi in v.dims)


def field_decl(tr, v, consts, proc=None):
    n = rs_name(v.cs_name)
    if v.is_array:
        return n, rs_type(v.type), f'VbArray::new(&[{bounds(tr, v, consts, proc)}])'
    return n, RS_TYPES[v.ctype], DEFAULTS[v.ctype]


class Fields:
    def __init__(self):
        self.fields = []                    # (name, type, initializer)
        self.consts = []


HEADER = [
    '// <auto-generated>',
    "// Translated from Killer Hearts' {source} by Transpiler/vb2rs.py in brianberns/Hearts.",
    '// Do not edit: change the translator and regenerate instead.',
    '// </auto-generated>',
]


def main():
    src, outdir = sys.argv[1], sys.argv[2]
    vb.SOURCE_PATCHES.extend(ARENA_PATCHES)
    modules = [vb.read_module(os.path.join(src, f), name, excl, incl) for f, name, excl, incl in SOURCES]
    tr = vb.Translator(modules)
    os.makedirs(outdir, exist_ok=True)

    # constants first, since locals may not shadow them in Rust
    fields = Fields()
    consts = set()
    for mod in modules:
        for d in mod.decls:
            for dv in d.decls:
                low = dv.name.lower()
                v = tr.globals.get(low) if (d.scope in ('public', 'global') or mod.name == 'Globals') \
                    else tr.module_vars.get((mod.name, low))
                if v.kind == 'const':
                    consts.add(rs_name(v.cs_name))
    for proc in tr.procs.values():
        for v in proc.locals.values():
            if v.kind == 'const':
                consts.add(rs_name(v.cs_name))

    for mod in modules:
        for d in mod.decls:
            for dv in d.decls:
                low = dv.name.lower()
                v = tr.globals.get(low) if (d.scope in ('public', 'global') or mod.name == 'Globals') \
                    else tr.module_vars.get((mod.name, low))
                comment = f'  // {d.comment}' if d.comment and dv is d.decls[-1] else ''
                if v.kind == 'const':
                    gen = RsGen(tr, None, consts)
                    fields.consts.append(f'pub const {rs_name(v.cs_name)}: {const_type(v.ctype)} = '
                                         f'{gen.const_expr(dv.value, v.ctype)};{comment}')
                else:
                    fields.fields.append(field_decl(tr, v, consts) + (comment,))

    impls = {}
    for mod in modules:
        lines = []
        for p in mod.procs:
            proc = tr.procs[(mod.name, p.name.lower())]
            if proc.excluded:
                continue
            gen = RsGen(tr, proc, consts)
            lines.append('')
            lines.extend(gen.generate(fields))
        impls[mod.name] = lines

    def source_of(name):
        return [f for f, n, _, _ in SOURCES if n == name][0]

    allow = f'#![allow({ALLOWS})]'
    with open(os.path.join(outdir, 'consts.rs'), 'w', encoding='utf-8', newline='\n') as f:
        f.write('\n'.join([h.format(source='constants') for h in HEADER] + ['', allow, ''] + fields.consts) + '\n')

    for name, lines in impls.items():
        if not lines:
            continue
        head = [h.format(source=source_of(name)) for h in HEADER] + [
            '', allow, '',
            'use super::consts::*;',
            'use super::KhEngine;',
            'use crate::utilities::*;',
            'use crate::vb::{self, VbArray};',
            '',
            'impl KhEngine {',
        ]
        with open(os.path.join(outdir, f'{name.lower()}.rs'), 'w', encoding='utf-8', newline='\n') as f:
            f.write('\n'.join(head + lines + ['}']) + '\n')

    struct = [h.format(source='modules') for h in HEADER] + [
        '', '//! The state of Killer Hearts\' engine: its globals, module variables, static locals and form',
        '//! controls, as one struct.', '',
        allow, '',
        'pub mod consts;',
    ] + [f'mod {n.lower()};' for n, lines in impls.items() if lines] + [
        '',
        'use consts::*;',
        'use crate::vb::VbArray;',
        '',
        'pub struct KhEngine {',
    ]
    entries = []
    for entry in fields.fields:
        n, t, init = entry[:3]
        comment = entry[3] if len(entry) > 3 else ''
        entries.append((n, t, init, comment))
    for field, t in sorted(tr.ui_fields.items()):
        entries.append((field, RS_TYPES[t], DEFAULTS[t], ''))
    for n, t, _, comment in entries:
        struct.append(f'    pub(crate) {n}: {t},{comment}')
    struct.append('    pub(crate) table: crate::table::TableState,')
    struct += ['}', '', 'impl KhEngine {', '    /// The engine as VB starts it: every variable empty.',
               '    pub(crate) fn blank() -> KhEngine {', '        KhEngine {']
    for n, _, init, _ in entries:
        struct.append(f'            {n}: {init},')
    struct += ['            table: Default::default(),', '        }', '    }', '}']
    with open(os.path.join(outdir, 'mod.rs'), 'w', encoding='utf-8', newline='\n') as f:
        f.write('\n'.join(struct) + '\n')

    for w in sorted(set(tr.warnings)):
        print('warning:', w, file=sys.stderr)


if __name__ == '__main__':
    main()
