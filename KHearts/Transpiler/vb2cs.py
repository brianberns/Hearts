#!/usr/bin/env python3
"""Translates the Killer Hearts VB6 engine into C#.

This handles only the subset of VB6 that Killer Hearts' engine uses, and aims to preserve its semantics
exactly: ByRef parameters, Static procedures (whose locals persist between calls), non-short-circuit
And/Or, True = -1, arrays with arbitrary lower bounds, banker's rounding when floats are assigned to
integers, and loop bounds evaluated once. The output is one C# partial class, KhEngine, with the
original comments preserved.

usage: vb2cs.py <KHearts source folder> <output folder>
"""

import os
import re
import sys

# ---------------------------------------------------------------------------------------------------
# What to translate

# (file, module name, procedures to exclude, procedures to include (None means all))
SOURCES = [
    ('Public_Constants_and_Variables.bas', 'Globals', set(), None),
    ('Playing.bas', 'Playing', None, None),     # exclusions computed below
    ('Passing.bas', 'Passing', {
        'determine_new_player_pass', 'determine_random_pass', 'determine_kind_of_pass_to_make',
        'pass_to_lie_low', 'pass_to_get_low_scorer'}, None),
    ('KHearts.frm', 'Form', set(), {
        'analyze_trick_so_far', 'update_void_records_and_other_parameters', 'update_arc_Shooter',
        'compute_public_parameters', 'pub_player_void', 'update_private_info',
        'update_pub_info_on_player', 'update_others_public_info_for_card', 'set_score_between_hands',
        'cards_higher_than', 'update_private_info_with_pass_information'}),
]

# Lines where the compiled program does not do what its source says, so the port follows the program.
# Each is (file, line as written, replacement); each must match exactly one line.
SOURCE_PATCHES = [
    # In select_best_card_for_situation's general sluff (sit = 14), the compiled program never takes this
    # branch: it always goes on to sluff high in a_live_suit. Both the 2020 build of this source and the
    # 2025 build of V7.03 do so in every one of more than 500 cases where suit_is_live(spades) is true and
    # QS_played is false, so the condition is effectively false there; why, the source doesn't say.
    ('Playing.bas',
     "ElseIf suit_is_live(spades) And Not QS_played Then 'null",
     "ElseIf False And (suit_is_live(spades) And Not QS_played) Then 'null (patched: see SOURCE_PATCHES)"),
]

# Playing.bas: only the best player is ported
def playing_excluded(name):
    n = name.lower()
    return n.startswith('btr_') or n.startswith('gp_') or n in ('script_for_new_player', 'script_for_random_player')

# Procedures written by hand in C# (Utilities.cs), with their signatures: (return type or None, param types, ref flags)
HANDWRITTEN = {
    'get_next_player': ('int', ['int'], [False]),
    'get_previous_player': ('int', ['int'], [False]),
    'get_index_from_rank_and_suit': (None, ['int', 'int', 'int'], [True, False, False]),
    'get_rank_and_suit_from_index': (None, ['int', 'int', 'int'], [False, True, True]),
    'mini': ('int', ['params'], [False]),
    'maxi': ('int', ['params'], [False]),
    'minr': ('float', ['params'], [False]),
    'maxr': ('float', ['params'], [False]),
    'count_bits_on': ('int', ['int'], [False]),
    'bit_numbers_on': ('string', ['int'], [False]),
    'format_I': ('string', ['int', 'int'], [False, False]),
    'format_F': ('string', ['float', 'int', 'int'], [False, False, False]),
    'name_string': ('string', [('arr', 'bool', 1)], [False]),
    'indirect_sort': (None, [('arr', 'int', 1), ('arr', 'int', 1)], [False, False]),
    'truth_map': ('int', ['bool', 'int', 'int'], [False, False, False]),
    'bb_add': (None, ['string'], [False]),
    'bb_cla': (None, ['string'], [False]),
    'display_center_message': (None, ['string', 'int'], [False, False]),
    'diagnose_and_halt': (None, ['string'], [False]),
    'capture_snapshot': (None, ['string'], [False]),
    'print_instantaneous_data': (None, [], []),
    'PlaySoundX': (None, ['string'], [False]),
    'turn_off_sound': (None, [], []),
}

CS_KEYWORDS = set('''abstract as base bool break byte case catch char checked class const continue decimal default
delegate do double else enum event explicit extern false finally fixed float for foreach goto if implicit in int
interface internal is lock long namespace new null object operator out override params private protected public
readonly ref return sbyte sealed short sizeof stackalloc static string struct switch this throw true try typeof uint
ulong unchecked unsafe ushort using virtual void volatile while'''.split())

VB_TYPES = {'long': 'int', 'integer': 'int', 'boolean': 'bool', 'single': 'float', 'double': 'double',
            'string': 'string', 'variant': 'object', 'byte': 'int', 'date': 'double'}

# ---------------------------------------------------------------------------------------------------
# Lexing

TOKEN_RE = re.compile(r'''
    (?P<str>"(?:[^"]|"")*")
  | (?P<hex>&H[0-9A-Fa-f]+&?)
  | (?P<num>(?:\d+\.\d*|\.\d+|\d+)(?:[eE][+-]?\d+)?[#!&%@]?)
  | (?P<id>[A-Za-z_][A-Za-z0-9_]*[$]?)
  | (?P<op><>|<=|>=|[=<>+\-*/\\^&(),.:\#;])
  | (?P<ws>\s+)
''', re.VERBOSE)


class Tok:
    def __init__(self, kind, text):
        self.kind, self.text = kind, text
        self.low = text.lower()

    def __repr__(self):
        return self.text

    def is_(self, *words):
        return self.kind in ('id', 'op') and self.low in words


def tokenize(code, where):
    toks, i = [], 0
    while i < len(code):
        m = TOKEN_RE.match(code, i)
        if not m:
            raise SyntaxError(f'{where}: cannot tokenize at: {code[i:]}')
        i = m.end()
        kind = m.lastgroup
        if kind != 'ws':
            toks.append(Tok(kind, m.group(kind)))
    return toks


def split_comment(line):
    """Splits a physical line into code and comment, respecting string literals."""
    in_str = False
    for i, c in enumerate(line):
        if c == '"':
            in_str = not in_str
        elif c == "'" and not in_str:
            return line[:i], line[i + 1:]
    return line, None


class LogicalLine:
    def __init__(self, code, comment, lineno):
        self.code, self.comment, self.lineno = code, comment, lineno


def logical_lines(lines, first_lineno):
    """Joins continuation lines, separating code from comments."""
    out, buf, comments, start = [], '', [], None
    for n, raw in enumerate(lines, first_lineno):
        code, comment = split_comment(raw.rstrip('\r\n'))
        if comment is not None and comment.strip():
            comments.append(comment.strip())
        if start is None:
            start = n
        stripped = code.rstrip()
        if stripped.endswith(' _') or stripped == '_':
            buf += stripped[:-1] + ' '
            continue
        buf += code
        out.append(LogicalLine(buf.strip(), ' '.join(comments) if comments else None, start))
        buf, comments, start = '', [], None
    return out

# ---------------------------------------------------------------------------------------------------
# AST

class Node:
    def __init__(self, kind, **kw):
        self.kind = kind
        self.__dict__.update(kw)

    def __repr__(self):
        return f'{self.kind}({", ".join(f"{k}={v!r}" for k, v in self.__dict__.items() if k != "kind")})'


def N(kind, **kw):
    return Node(kind, **kw)

# ---------------------------------------------------------------------------------------------------
# Expression parsing

class TokStream:
    def __init__(self, toks, where):
        self.toks, self.i, self.where = toks, 0, where

    def peek(self, k=0):
        j = self.i + k
        return self.toks[j] if j < len(self.toks) else None

    def next(self):
        t = self.peek()
        self.i += 1
        return t

    def at(self, *words):
        t = self.peek()
        return t is not None and t.is_(*words)

    def expect(self, *words):
        t = self.next()
        if t is None or not t.is_(*words):
            raise SyntaxError(f'{self.where}: expected {words}, got {t}')
        return t

    def done(self):
        return self.i >= len(self.toks)


BINARY_LEVELS = [
    ['imp'], ['eqv'], ['xor'], ['or'], ['and'],
    None,                                  # Not (unary)
    ['=', '<>', '<', '>', '<=', '>=', 'like', 'is'],
    ['&'], ['+', '-'], ['mod'], ['\\'], ['*', '/'],
    'neg',                                 # unary minus
    ['^'],
]


def parse_expr(ts, level=0):
    if level >= len(BINARY_LEVELS):
        return parse_primary(ts)
    ops = BINARY_LEVELS[level]
    if ops is None:                        # Not
        if ts.at('not'):
            ts.next()
            return N('unary', op='not', e=parse_expr(ts, level))
        return parse_expr(ts, level + 1)
    if ops == 'neg':
        if ts.at('-'):
            ts.next()
            return N('unary', op='-', e=parse_expr(ts, level))
        if ts.at('+'):
            ts.next()
            return parse_expr(ts, level)
        return parse_expr(ts, level + 1)
    left = parse_expr(ts, level + 1)
    while ts.peek() is not None and ts.peek().is_(*ops):
        op = ts.next().low
        if op == '^':
            right = parse_expr(ts, level - 1)     # allow unary minus in exponent
        else:
            right = parse_expr(ts, level + 1)
        left = N('bin', op=op, a=left, b=right)
    return left


def parse_args(ts):
    """Parses a parenthesized argument list; the opening paren is current."""
    ts.expect('(')
    args = []
    if ts.at(')'):
        ts.next()
        return args
    while True:
        args.append(parse_expr(ts))
        if ts.at(','):
            ts.next()
            continue
        ts.expect(')')
        return args


def parse_primary(ts):
    t = ts.next()
    if t is None:
        raise SyntaxError(f'{ts.where}: unexpected end of expression')
    if t.kind == 'str':
        return N('str', v=t.text[1:-1].replace('""', '"'))
    if t.kind == 'hex':
        text = t.text[2:]
        long_suffix = text.endswith('&')
        v = int(text.rstrip('&'), 16)
        if not long_suffix and 0x8000 <= v <= 0xFFFF:
            v -= 0x10000                   # an Integer literal
        elif v >= 0x80000000:
            v -= 0x100000000
        return N('num', v=v, t='int')
    if t.kind == 'num':
        text = t.text
        suffix = text[-1] if text[-1] in '#!&%@' else ''
        body = text[:-1] if suffix else text
        if (suffix and suffix in '#!') or '.' in body or 'e' in body.lower():
            return N('num', v=float(body), t='float' if suffix == '!' else 'double')
        return N('num', v=int(body), t='int')
    if t.is_('('):
        e = parse_expr(ts)
        ts.expect(')')
        return N('paren', e=e)
    if t.kind == 'id':
        node = N('name', name=t.text)
        if ts.at('('):
            node = N('index', name=t.text, args=parse_args(ts))
        while ts.at('.'):
            ts.next()
            member = ts.next().text
            node = N('member', obj=node, name=member)
            if ts.at('('):
                node.args = parse_args(ts)
        return node
    raise SyntaxError(f'{ts.where}: unexpected token {t}')

# ---------------------------------------------------------------------------------------------------
# Statement parsing (one logical line into a list of statement fragments)

def parse_decl_list(ts, where):
    """Parses 'x As Long, y(1 To 3) As Boolean, ...' into declarations."""
    decls = []
    while not ts.done():
        name = ts.next().text
        dims = None
        if ts.at('('):
            ts.next()
            dims = []
            while not ts.at(')'):
                lo = parse_expr(ts)
                if ts.at('to'):
                    ts.next()
                    hi = parse_expr(ts)
                else:
                    lo, hi = N('num', v=0, t='int'), lo
                dims.append((lo, hi))
                if ts.at(','):
                    ts.next()
            ts.expect(')')
        vtype, length = None, None
        if ts.at('as'):
            ts.next()
            vtype = ts.next().low
            if ts.at('*'):
                ts.next()
                length = ts.next().text
        value = None
        if ts.at('='):
            ts.next()
            value = parse_expr(ts)
        decls.append(N('decl', name=name, dims=dims, vtype=vtype, value=value))
        if ts.at(','):
            ts.next()
        elif not ts.done():
            raise SyntaxError(f'{where}: bad declaration near {ts.peek()}')
    return decls


def parse_params(ts, where):
    params = []
    ts.expect('(')
    while not ts.at(')'):
        byval, paramarray, optional = False, False, False
        while ts.at('byval', 'byref', 'paramarray', 'optional'):
            w = ts.next().low
            byval |= w == 'byval'
            paramarray |= w == 'paramarray'
            optional |= w == 'optional'
        name = ts.next().text
        is_array = False
        if ts.at('('):
            ts.next()
            ts.expect(')')
            is_array = True
        vtype = None
        if ts.at('as'):
            ts.next()
            vtype = ts.next().low
        params.append(N('param', name=name, byval=byval, paramarray=paramarray, is_array=is_array, vtype=vtype))
        if ts.at(','):
            ts.next()
    ts.expect(')')
    return params


def split_top(toks, sep_words):
    """Splits a token list at top-level separator tokens (outside parentheses)."""
    parts, cur, depth = [], [], 0
    for t in toks:
        if t.is_('('):
            depth += 1
        elif t.is_(')'):
            depth -= 1
        if depth == 0 and t.is_(*sep_words):
            parts.append(cur)
            cur = []
        else:
            cur.append(t)
    parts.append(cur)
    return parts


def find_top(toks, start, *words):
    depth = 0
    for j in range(start, len(toks)):
        t = toks[j]
        if t.is_('('):
            depth += 1
        elif t.is_(')'):
            depth -= 1
        elif depth == 0 and t.is_(*words):
            return j
    return -1


def parse_simple(toks, where):
    """Parses one simple (non-block) statement from a token list."""
    if not toks:
        return None
    ts = TokStream(toks, where)
    t0 = toks[0]
    w = t0.low if t0.kind == 'id' else None
    if w == 'call':
        ts.next()
        name = ts.next().text
        args = parse_args(ts) if ts.at('(') else []
        node = N('call', name=name, args=args)
        while ts.at('.'):                      # Call Red_Felt.x  (UI)
            ts.next()
            ts.next()
            node = N('ui', text=' '.join(t.text for t in toks))
        return node
    if w == 'exit':
        return N('exit', what=toks[1].low)
    if w == 'erase':
        ts.next()
        names = [p[0].text for p in split_top(toks[1:], [','])]
        return N('erase', names=names)
    if w in ('print', 'open', 'close', 'put', 'get', 'input', 'doevents', 'sendkeys', 'beep'):
        return N('nop', text=' '.join(t.text for t in toks))
    if w == 'end' and len(toks) == 1:
        return N('end')
    if w == 'goto':
        return N('goto', label=toks[1].text)
    if w in ('dim', 'static', 'private', 'public', 'const', 'global') and len(toks) > 1:
        is_const = False
        while ts.at('dim', 'static', 'private', 'public', 'const', 'global'):
            is_const |= ts.next().low == 'const'
        return N('dim', decls=parse_decl_list(ts, where), const=is_const, scope=w)
    # assignment or call
    eq = find_top(toks, 0, '=')
    if eq > 0:
        lts = TokStream(toks[:eq], where)
        lhs = parse_primary(lts)
        if not lts.done():
            raise SyntaxError(f'{where}: bad assignment target')
        rts = TokStream(toks[eq + 1:], where)
        rhs = parse_expr(rts)
        if not rts.done():
            raise SyntaxError(f'{where}: trailing tokens in assignment: {rts.toks[rts.i:]}')
        return N('assign', lhs=lhs, rhs=rhs)
    # call without Call keyword: name [args]
    if t0.kind != 'id':
        raise SyntaxError(f'{where}: cannot parse statement: {toks}')
    lead = TokStream(toks, where)
    try:
        if parse_primary(lead).kind == 'member':
            return N('ui', text=' '.join(t.text for t in toks))      # a form control's method
    except SyntaxError:
        pass
    if len(toks) > 1 and toks[1].is_('(') and find_top(toks, 1, ')') == len(toks) - 1 and len(split_top(toks[2:-1], [','])) == 1:
        # name (expr): one parenthesized argument, passed by value
        ets = TokStream(toks[1:], where)
        arg = parse_primary(ets)
        if ets.done():
            return N('call', name=t0.text, args=[arg])
    ats = TokStream(toks[1:], where)
    args = []
    while not ats.done():
        args.append(parse_expr(ats))
        if ats.at(','):
            ats.next()
        elif not ats.done():
            raise SyntaxError(f'{where}: bad call arguments: {toks}')
    return N('call', name=t0.text, args=args)


def parse_line_stmts(toks, where, allow_blocks=True):
    """Parses a logical line's tokens into statement fragments (block headers stay separate)."""
    out = []
    i = 0
    n = len(toks)
    while i < n:
        t = toks[i]
        if t.is_(':'):
            i += 1
            continue
        w = t.low if t.kind == 'id' else None
        if w == 'if':
            j = find_top(toks, i + 1, 'then')
            cond = parse_expr(TokStream(toks[i + 1:j], where))
            rest = toks[j + 1:]
            if not rest:
                out.append(N('ifblock', cond=cond))
                return out
            # single-line If: then-part runs to a top-level Else, else-part to the end of the line
            e = find_else(rest)
            then_toks = rest if e < 0 else rest[:e]
            else_toks = [] if e < 0 else rest[e + 1:]
            then_s = parse_line_stmts(then_toks, where, allow_blocks=False)
            else_s = parse_line_stmts(else_toks, where, allow_blocks=False)
            out.append(N('ifline', cond=cond, then=then_s, els=else_s))
            return out
        if w == 'elseif' and allow_blocks:
            j = find_top(toks, i + 1, 'then')
            out.append(N('elseif', cond=parse_expr(TokStream(toks[i + 1:j], where))))
            i = j + 1
            continue
        if w == 'else' and allow_blocks:
            out.append(N('else'))
            i += 1
            continue
        if w == 'end' and i + 1 < n and toks[i + 1].low in ('if', 'select', 'sub', 'function'):
            out.append(N('end_' + toks[i + 1].low))
            i += 2
            continue
        if w == 'select' and i + 1 < n and toks[i + 1].low == 'case':
            out.append(N('select', e=parse_expr(TokStream(toks[i + 2:], where))))
            return out
        if w == 'case' and allow_blocks:
            j = find_top(toks, i + 1, ':')
            body = toks[i + 1:] if j < 0 else toks[i + 1:j]
            if body and body[0].low == 'else':
                out.append(N('caseelse'))
            else:
                items = []
                for part in split_top(body, [',']):
                    if part and part[0].low == 'is':
                        items.append(('is', part[1].text, parse_expr(TokStream(part[2:], where))))
                    else:
                        k = find_top(part, 0, 'to')
                        if k >= 0:
                            items.append(('range', parse_expr(TokStream(part[:k], where)),
                                          parse_expr(TokStream(part[k + 1:], where))))
                        else:
                            items.append(('eq', parse_expr(TokStream(part, where))))
                out.append(N('case', items=items))
            if j < 0:
                return out
            i = j + 1
            continue
        if w == 'for' and i + 1 < n and toks[i + 1].low != 'each':
            j = find_top(toks, i + 1, ':')
            seg = toks[i + 1:] if j < 0 else toks[i + 1:j]
            k = find_top(seg, 0, 'to')
            eq = find_top(seg, 0, '=')
            var = parse_primary(TokStream(seg[:eq], where))
            start = parse_expr(TokStream(seg[eq + 1:k], where))
            st = find_top(seg, k + 1, 'step')
            if st >= 0:
                end = parse_expr(TokStream(seg[k + 1:st], where))
                step = parse_expr(TokStream(seg[st + 1:], where))
            else:
                end = parse_expr(TokStream(seg[k + 1:], where))
                step = None
            out.append(N('for', var=var, start=start, end=end, step=step))
            if j < 0:
                return out
            i = j + 1
            continue
        if w == 'next':
            out.append(N('next'))
            i += 1
            while i < n and not toks[i].is_(':'):
                i += 1                      # Next i
            continue
        if w == 'do':
            j = find_top(toks, i + 1, ':')
            seg = toks[i + 1:] if j < 0 else toks[i + 1:j]
            if seg:
                out.append(N('do', how=seg[0].low, cond=parse_expr(TokStream(seg[1:], where))))
            else:
                out.append(N('do', how=None, cond=None))
            if j < 0:
                return out
            i = j + 1
            continue
        if w == 'loop':
            j = find_top(toks, i + 1, ':')
            seg = toks[i + 1:] if j < 0 else toks[i + 1:j]
            if seg:
                out.append(N('loop', how=seg[0].low, cond=parse_expr(TokStream(seg[1:], where))))
            else:
                out.append(N('loop', how=None, cond=None))
            if j < 0:
                return out
            i = j + 1
            continue
        if w in ('sub', 'function', 'static', 'private', 'public', 'friend') and is_proc_header(toks, i):
            out.append(parse_proc_header(toks[i:], where))
            return out
        if i == 0 and t.kind == 'id' and n > 1 and toks[1].is_(':') and w not in ('else',) and allow_blocks:
            out.append(N('label', name=t.text))
            i += 2
            continue
        # simple statement up to next top-level ':'
        j = find_top(toks, i, ':')
        seg = toks[i:] if j < 0 else toks[i:j]
        s = parse_simple(seg, where)
        if s is not None:
            out.append(s)
        if j < 0:
            return out
        i = j + 1
    return out


def find_else(toks):
    """Finds the Else of a single-line If, skipping those of nested single-line Ifs."""
    depth, nested = 0, 0
    for j, t in enumerate(toks):
        if t.is_('('):
            depth += 1
        elif t.is_(')'):
            depth -= 1
        elif depth == 0 and t.is_('if'):
            nested += 1
        elif depth == 0 and t.is_('else'):
            if nested == 0:
                return j
            nested -= 1
    return -1


def is_proc_header(toks, i):
    j = i
    while j < len(toks) and toks[j].low in ('static', 'private', 'public', 'friend'):
        j += 1
    return j < len(toks) and toks[j].low in ('sub', 'function')


def parse_proc_header(toks, where):
    ts = TokStream(toks, where)
    static, scope = False, 'public'
    while ts.at('static', 'private', 'public', 'friend'):
        w = ts.next().low
        static |= w == 'static'
        if w in ('private', 'public'):
            scope = w
    kind = ts.next().low
    name = ts.next().text
    params = parse_params(ts, where) if ts.at('(') else []
    rtype = None
    if ts.at('as'):
        ts.next()
        rtype = ts.next().low
    return N('proc', pkind=kind, name=name, params=params, rtype=rtype, static=static, scope=scope)

# ---------------------------------------------------------------------------------------------------
# Block structure

class Module:
    def __init__(self, name):
        self.name = name
        self.decls = []          # module-level dim nodes, with comments
        self.procs = []
        self.header_comments = []


def read_module(path, modname, excluded, included):
    with open(path, encoding='cp1252') as f:
        lines = f.readlines()
    for file, original, replacement in SOURCE_PATCHES:
        if file != os.path.basename(path):
            continue
        hits = [k for k, l in enumerate(lines) if l.strip() == original]
        if len(hits) != 1:
            raise ValueError(f'{file}: patch matches {len(hits)} lines: {original}')
        k = hits[0]
        lines[k] = lines[k][:len(lines[k]) - len(lines[k].lstrip())] + replacement + '\n'
    first = 0
    if path.lower().endswith('.frm'):
        first = next(k for k, l in enumerate(lines) if l.startswith('Option Explicit'))
    lines = lines[first:]
    mod = Module(modname)
    llines = logical_lines(lines, first + 1)

    # gather each line's fragments, with its comment, skipping procedures not included
    frags = []
    skipping = False
    header_re = re.compile(r'^(?:(?:Public|Private|Static|Friend)\s+)*(?:Sub|Function)\s+(\w+)', re.I)
    for ll in llines:
        where = f'{os.path.basename(path)}:{ll.lineno}'
        m = header_re.match(ll.code)
        if m and included is not None and m.group(1) not in included:
            skipping = True
            continue
        if skipping:
            if re.match(r'^End\s+(Sub|Function)\b', ll.code, re.I):
                skipping = False
            continue
        if not ll.code:
            frags.append((N('comment', text=ll.comment), ll))
            continue
        low = ll.code.lower()
        if low.startswith('attribute ') or low.startswith('option ') or low.startswith('defdate') \
                or low.startswith('declare ') or ' declare ' in low[:20]:
            continue
        toks = tokenize(ll.code, where)
        stmts = parse_line_stmts(toks, where)
        for k, s in enumerate(stmts):
            s.lineno = ll.lineno
            s.where = where
            frags.append((s, ll if k == len(stmts) - 1 else None))

    # split into module declarations and procedures
    k = 0
    pending_comments = []
    while k < len(frags):
        s, ll = frags[k]
        if s.kind == 'proc':
            body, k = collect_proc_body(frags, k + 1)
            proc = s
            proc.comment = ll.comment if ll else None
            proc.body = build_blocks(body, proc.name)
            proc.lead_comments = pending_comments
            pending_comments = []
            name = proc.name
            if included is not None and name not in included:
                continue
            if excluded is not None and name in excluded:
                proc.excluded = True
            elif excluded is None and modname == 'Playing' and playing_excluded(name):
                proc.excluded = True
            else:
                proc.excluded = False
            proc.module = modname
            mod.procs.append(proc)
            continue
        if s.kind == 'comment':
            if not mod.procs and included is None:
                mod.header_comments.append(s.text)
            pending_comments.append(s.text)
        elif s.kind == 'dim':
            s.comment = ll.comment if ll else None
            mod.decls.append(s)
            pending_comments = []
        k += 1
    return mod


def collect_proc_body(frags, k):
    body = []
    while k < len(frags):
        s, ll = frags[k]
        if s.kind in ('end_sub', 'end_function'):
            return body, k + 1
        s.comment = ll.comment if (ll and s.kind != 'comment') else getattr(s, 'comment', None)
        if s.kind == 'comment':
            s.comment = None
        body.append(s)
        k += 1
    raise SyntaxError('unterminated procedure')


def build_blocks(frags, procname):
    """Builds nested block statements from the flat fragment list."""
    pos = 0

    def block(terminators):
        nonlocal pos
        stmts = []
        while pos < len(frags):
            s = frags[pos]
            if s.kind in terminators:
                return stmts
            pos += 1
            if s.kind == 'ifblock':
                node = N('if', clauses=[(s.cond, None)], els=None, lineno=s.lineno, comment=s.comment)
                body = block(('elseif', 'else', 'end_if'))
                node.clauses[0] = (s.cond, body)
                clause_comments = []
                while True:
                    t = frags[pos]
                    pos += 1
                    if t.kind == 'elseif':
                        b = block(('elseif', 'else', 'end_if'))
                        node.clauses.append((t.cond, b))
                        clause_comments.append(t.comment)
                    elif t.kind == 'else':
                        node.els = block(('end_if',))
                        node.else_comment = t.comment
                    elif t.kind == 'end_if':
                        break
                node.clause_comments = clause_comments
                stmts.append(node)
            elif s.kind == 'select':
                node = N('selectcase', e=s.e, cases=[], els=None, lineno=s.lineno, comment=s.comment)
                # skip comments before first Case
                while frags[pos].kind == 'comment':
                    stmts.append(frags[pos])
                    pos += 1
                while True:
                    t = frags[pos]
                    pos += 1
                    if t.kind == 'case':
                        b = block(('case', 'caseelse', 'end_select'))
                        node.cases.append((t.items, b, t.comment))
                    elif t.kind == 'caseelse':
                        node.els = block(('case', 'caseelse', 'end_select'))
                        node.else_comment = t.comment
                    elif t.kind == 'end_select':
                        break
                stmts.append(node)
            elif s.kind == 'for':
                s.body = block(('next',))
                pos += 1
                stmts.append(s)
            elif s.kind == 'do':
                s.body = block(('loop',))
                t = frags[pos]
                pos += 1
                s.post_how, s.post_cond = t.how, t.cond
                stmts.append(s)
            elif s.kind in ('elseif', 'else', 'end_if', 'case', 'caseelse', 'end_select', 'next', 'loop'):
                raise SyntaxError(f'{s.where}: unexpected {s.kind} in {procname}')
            else:
                stmts.append(s)
        return stmts

    result = block(())
    if pos != len(frags):
        raise SyntaxError(f'{procname}: unparsed fragments remain')
    return result

# ---------------------------------------------------------------------------------------------------
# Symbols and types

def vb_type(vtype, default='double'):
    if vtype is None:
        return default                     # DefDate A-Z: untyped means Date, a double
    return VB_TYPES.get(vtype, 'object')


class Var:
    def __init__(self, name, ctype, dims, cs_name, kind, module, const_value=None):
        self.name, self.ctype, self.dims, self.cs_name = name, ctype, dims, cs_name
        self.kind = kind                   # 'global', 'module', 'local', 'param', 'const', 'ret'
        self.module = module
        self.const_value = const_value
        self.is_array = dims is not None

    @property
    def type(self):
        return ('arr', self.ctype, len(self.dims)) if self.is_array else self.ctype


class Proc:
    def __init__(self, node, cs_name):
        self.node = node
        self.name = node.name
        self.cs_name = cs_name
        self.module = node.module
        self.is_function = node.pkind == 'function'
        self.rtype = vb_type(node.rtype) if self.is_function else None
        self.params = node.params
        self.static = node.static
        self.private = node.scope == 'private'
        self.excluded = node.excluded
        self.ref_params = [False] * len(node.params)
        self.locals = {}


def cs_ident(name):
    return '@' + name if name in CS_KEYWORDS else name


class Translator:
    def __init__(self, modules):
        self.modules = modules
        self.globals = {}                  # lower name -> Var (public, all modules)
        self.module_vars = {}              # (module, lower) -> Var
        self.procs = {}                    # (module, lower) -> Proc
        self.public_procs = {}             # lower -> Proc
        self.handwritten = {k.lower(): v for k, v in HANDWRITTEN.items()}
        self.handwritten_names = {k.lower(): k for k in HANDWRITTEN}
        self.ui_fields = {}
        self.warnings = []
        self.temp = 0
        self.label = 0
        self.collect()

    def warn(self, msg):
        self.warnings.append(msg)

    def collect(self):
        # names declared in more than one module (or both privately and publicly) get a module prefix
        counts = {}
        for mod in self.modules:
            for d in mod.decls:
                for v in d.decls:
                    counts.setdefault(v.name.lower(), set()).add(mod.name)
        proc_counts = {}
        for mod in self.modules:
            for p in mod.procs:
                proc_counts.setdefault(p.name.lower(), set()).add(mod.name)
        for mod in self.modules:
            for d in mod.decls:
                public = d.scope in ('public', 'global') or mod.name == 'Globals'
                for v in d.decls:
                    low = v.name.lower()
                    ctype = vb_type(v.vtype, 'double')
                    if d.const and v.vtype is None:
                        ctype = self.const_type(v.value)
                    clash = len(counts[low]) > 1 and not public
                    cs = f'{mod.name}_{v.name}' if clash else v.name
                    var = Var(v.name, ctype, v.dims, cs_ident(cs), 'const' if d.const else ('global' if public else 'module'),
                              mod.name, v.value)
                    var.decl = v
                    if public:
                        self.globals[low] = var
                    else:
                        self.module_vars[(mod.name, low)] = var
            for p in mod.procs:
                low = p.name.lower()
                cs = f'{mod.name}_{p.name}' if len(proc_counts[low]) > 1 else p.name
                proc = Proc(p, cs_ident(cs))
                self.procs[(mod.name, low)] = proc
                if not proc.private:
                    self.public_procs.setdefault(low, proc)
        for proc in self.procs.values():
            self.collect_locals(proc)
        self.infer_ref_params()

    def const_type(self, value):
        if value.kind == 'str':
            return 'string'
        if value.kind == 'num':
            return value.t
        return 'int'

    def collect_locals(self, proc):
        owner = proc.cs_name.lstrip('@')
        for i, p in enumerate(proc.params):
            ctype = vb_type(p.vtype)
            dims = [None] if p.is_array else None
            v = Var(p.name, ctype, dims, cs_ident(p.name), 'param', proc.module)
            v.param_index = i
            v.paramarray = p.paramarray
            proc.locals[p.name.lower()] = v

        def walk(stmts):
            for s in stmts:
                if s.kind == 'dim':
                    for d in s.decls:
                        if s.const:
                            ctype = vb_type(d.vtype) if d.vtype else self.const_type(d.value)
                            v = Var(d.name, ctype, None, cs_ident(owner + '_' + d.name), 'const', proc.module, d.value)
                            v.decl = d
                            proc.locals[d.name.lower()] = v
                            continue
                        static_local = proc.static or s.scope == 'static'
                        cs = (owner + '_' + d.name) if static_local else d.name
                        v = Var(d.name, vb_type(d.vtype), d.dims, cs_ident(cs), 'static' if static_local else 'local',
                                proc.module)
                        v.decl = d
                        proc.locals[d.name.lower()] = v
                for b in children(s):
                    walk(b)

        walk(proc.node.body)
        if proc.is_function:
            proc.ret = Var(proc.name, proc.rtype, None, '__ret', 'ret', proc.module)

    def infer_ref_params(self):
        # a parameter is ByRef in C# if its procedure assigns it, or passes it on to a ByRef parameter
        changed = True
        while changed:
            changed = False
            for proc in self.procs.values():
                for i, p in enumerate(proc.params):
                    if proc.ref_params[i] or p.byval or p.is_array or p.paramarray:
                        continue
                    if self.assigns(proc, proc.node.body, p.name.lower()):
                        proc.ref_params[i] = True
                        changed = True

    def assigns(self, proc, stmts, low):
        for s in stmts:
            if s.kind == 'assign' and s.lhs.kind == 'name' and s.lhs.name.lower() == low:
                return True
            if s.kind == 'for' and s.var.kind == 'name' and s.var.name.lower() == low:
                return True
            if s.kind == 'call':
                callee = self.find_proc(proc.module, s.name)
                for j, a in enumerate(s.args):
                    if a.kind == 'name' and a.name.lower() == low and callee is not None and self.param_is_ref(callee, j):
                        return True
            for b in children(s):
                if self.assigns(proc, b, low):
                    return True
        return False

    def param_is_ref(self, callee, j):
        if isinstance(callee, Proc):
            return j < len(callee.ref_params) and callee.ref_params[j]
        _, ptypes, refs = callee
        return j < len(refs) and refs[j]

    def find_proc(self, module, name):
        low = name.lower()
        if (module, low) in self.procs:
            return self.procs[(module, low)]
        if low in self.public_procs:
            return self.public_procs[low]
        if low in self.handwritten:
            return self.handwritten[low]
        return None

    def lookup_var(self, proc, name):
        low = name.lower()
        if proc is not None:
            if low in proc.locals:
                return proc.locals[low]
            if proc.is_function and low == proc.name.lower():
                return proc.ret
            if (proc.module, low) in self.module_vars:
                return self.module_vars[(proc.module, low)]
        if low in self.globals:
            return self.globals[low]
        return None


def children(s):
    if s.kind == 'if':
        out = [b for _, b in s.clauses]
        if s.els is not None:
            out.append(s.els)
        return out
    if s.kind == 'ifline':
        return [s.then, s.els]
    if s.kind == 'selectcase':
        out = [b for _, b, _ in s.cases]
        if s.els is not None:
            out.append(s.els)
        return out
    if s.kind in ('for', 'do'):
        return [s.body]
    return []

# ---------------------------------------------------------------------------------------------------
# Code generation

BUILTIN_CONSTS = {'true': ('true', 'bool'), 'false': ('false', 'bool'), 'vbnullstring': ('""', 'string'),
                  'vbcrlf': ('"\\r\\n"', 'string'), 'vbcr': ('"\\r"', 'string'), 'vblf': ('"\\n"', 'string'),
                  'vbwhite': ('0xFFFFFF', 'int'), 'vbgreen': ('0x00FF00', 'int'), 'vbyellow': ('0x00FFFF', 'int'),
                  'vbblack': ('0', 'int'), 'vbbuttonface': ('0', 'int'), 'vbred': ('0x0000FF', 'int'),
                  'vbblue': ('0xFF0000', 'int')}

NUMERIC_RANK = {'int': 0, 'float': 1, 'double': 2}


def conv(code, frm, to):
    """Converts an expression of type frm to type to, VB6-style."""
    if frm == to or to is None or frm is None:
        return code
    if isinstance(to, tuple) or isinstance(frm, tuple):
        return code
    if to == 'object':
        return code
    if frm == 'object':
        return f'VB.To{to.capitalize()}({code})'
    if to == 'int':
        if frm == 'bool':
            return f'VB.B2I({code})'
        if frm in ('float', 'double'):
            return f'VB.CLng({code})'
        if frm == 'string':
            return f'VB.CLng(VB.Val({code}))'
    if to == 'bool':
        if frm in ('int', 'float', 'double'):
            return f'({code} != 0)'
        if frm == 'string':
            return f'VB.CBool({code})'
    if to == 'float':
        if frm == 'bool':
            return f'(float)VB.B2I({code})'
        if frm in ('int', 'double'):
            return f'(float)({code})'
        if frm == 'string':
            return f'(float)VB.Val({code})'
    if to == 'double':
        if frm == 'bool':
            return f'(double)VB.B2I({code})'
        if frm in ('int', 'float'):
            return f'(double)({code})'
        if frm == 'string':
            return f'VB.Val({code})'
    if to == 'string':
        return f'VB.CStr({code})'
    return code


class ProcGen:
    """Generates C# for one procedure."""

    def __init__(self, tr, proc):
        self.tr, self.proc = tr, proc
        self.out = []
        self.indent = 2
        self.loops = []                     # stack of (kind, exit label or None)
        self.used_labels = set()
        self.pre = []                       # statements hoisted out of the current expression

    def emit(self, line, comment=None):
        text = '    ' * self.indent + line
        if comment:
            text += ('  ' if line else '') + '// ' + comment
        self.out.append(text)

    def tmp(self):
        self.tr.temp += 1
        return f'__t{self.tr.temp}'

    # ---- expressions

    def expr(self, e, want=None):
        code, t = self.ex(e)
        return conv(code, t, want) if want else code

    def ex(self, e):
        k = e.kind
        if k == 'num':
            if e.t == 'int':
                return (str(e.v) if e.v >= 0 else f'({e.v})'), 'int'
            v = repr(float(e.v))
            if 'e' in v or 'E' in v:
                v = f'{e.v:.17g}'
            return (v + ('f' if e.t == 'float' else '')), e.t
        if k == 'str':
            return '"' + e.v.replace('\\', '\\\\').replace('"', '\\"') + '"', 'string'
        if k == 'paren':
            c, t = self.ex(e.e)
            return f'({c})', t
        if k == 'name':
            return self.ex_name(e.name)
        if k == 'index':
            return self.ex_index(e)
        if k == 'member':
            return self.ex_member(e)
        if k == 'unary':
            c, t = self.ex(e.e)
            if e.op == '-':
                if t == 'bool':
                    return f'(-VB.B2I({c}))', 'int'
                return f'(-{c})', t
            if t == 'bool':
                return f'(!{c})', 'bool'
            return f'(~{conv(c, t, "int")})', 'int'
        if k == 'bin':
            return self.ex_bin(e)
        raise ValueError(f'unknown expression {e}')

    def ex_name(self, name):
        low = name.lower()
        v = self.tr.lookup_var(self.proc, name)
        if v is not None:
            return v.cs_name, v.type
        if low in BUILTIN_CONSTS:
            return BUILTIN_CONSTS[low]
        callee = self.tr.find_proc(self.proc.module, name)
        if callee is not None:
            return self.call_code(callee, name, [])
        raise ValueError(f'{self.proc.name}: unknown name {name}')

    def ex_index(self, e):
        v = self.tr.lookup_var(self.proc, e.name)
        if v is not None and v.is_array:
            idx = ', '.join(self.expr(a, 'int') for a in e.args)
            return f'{v.cs_name}[{idx}]', v.ctype
        low = e.name.lower()
        if low in ('int', 'fix'):
            c, t = self.ex(e.args[0])
            return f'VB.{e.name.capitalize()}({conv(c, t, "double")})', 'double'
        if low in ('csng', 'cdbl', 'clng', 'cint', 'cbool', 'cstr'):
            target = {'csng': 'float', 'cdbl': 'double', 'clng': 'int', 'cint': 'int', 'cbool': 'bool', 'cstr': 'string'}[low]
            c, t = self.ex(e.args[0])
            return conv(c, t, target), target
        if low == 'abs':
            c, t = self.ex(e.args[0])
            return f'Math.Abs({c})', t
        if low == 'sgn':
            c, t = self.ex(e.args[0])
            return f'Math.Sign({c})', 'int'
        if low == 'len':
            return f'VB.Len({self.expr(e.args[0], "string")})', 'int'
        if low == 'mid':
            args = [self.expr(e.args[0], 'string')] + [self.expr(a, 'int') for a in e.args[1:]]
            return f'VB.Mid({", ".join(args)})', 'string'
        if low in ('left', 'right'):
            return f'VB.{e.name.capitalize()}({self.expr(e.args[0], "string")}, {self.expr(e.args[1], "int")})', 'string'
        callee = self.tr.find_proc(self.proc.module, e.name)
        if callee is not None:
            return self.call_code(callee, e.name, e.args)
        if v is not None:
            raise ValueError(f'{self.proc.name}: {e.name} is not an array')
        raise ValueError(f'{self.proc.name}: unknown function or array {e.name}')

    def ex_member(self, e):
        # form controls and their properties (UI) become stub fields
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
        t = 'bool' if prop in ('checked', 'visible', 'enabled', 'value') else ('string' if prop in ('caption', 'text') else 'int')
        self.tr.ui_fields[field] = t
        return field, t

    def ex_bin(self, e):
        op = e.op
        a, ta = self.ex(e.a)
        b, tb = self.ex(e.b)
        if op in ('and', 'or', 'xor'):
            sym = {'and': '&', 'or': '|', 'xor': '^'}[op]
            if ta == 'bool' and tb == 'bool':
                return f'({a} {sym} {b})', 'bool'
            return f'({conv(a, ta, "int")} {sym} {conv(b, tb, "int")})', 'int'
        if op in ('=', '<>', '<', '>', '<=', '>='):
            sym = {'=': '==', '<>': '!='}.get(op, op)
            if ta == 'string' or tb == 'string':
                if op in ('=', '<>'):
                    return f'({conv(a, ta, "string")} {sym} {conv(b, tb, "string")})', 'bool'
                return f'(string.CompareOrdinal({conv(a, ta, "string")}, {conv(b, tb, "string")}) {op} 0)', 'bool'
            if ta == 'bool' and tb == 'bool':
                if op in ('=', '<>'):
                    return f'({a} {sym} {b})', 'bool'
            t = self.numeric(ta, tb)
            return f'({conv(a, ta, t)} {sym} {conv(b, tb, t)})', 'bool'
        if op == '&':
            return f'({conv(a, ta, "string")} + {conv(b, tb, "string")})', 'string'
        if op == '/':
            return f'((double)({conv(a, ta, "double")}) / ({conv(b, tb, "double")}))', 'double'
        if op == '\\':
            return f'({conv(a, ta, "int")} / {conv(b, tb, "int")})', 'int'
        if op == 'mod':
            return f'({conv(a, ta, "int")} % {conv(b, tb, "int")})', 'int'
        if op == '^':
            return f'Math.Pow({conv(a, ta, "double")}, {conv(b, tb, "double")})', 'double'
        if op in ('+', '-', '*'):
            if op == '+' and (ta == 'string' or tb == 'string'):
                return f'({conv(a, ta, "string")} + {conv(b, tb, "string")})', 'string'
            t = self.numeric(ta, tb)
            return f'({conv(a, ta, t)} {op} {conv(b, tb, t)})', t
        raise ValueError(f'unsupported operator {op}')

    def numeric(self, ta, tb):
        ra = NUMERIC_RANK.get(ta, 0)
        rb = NUMERIC_RANK.get(tb, 0)
        r = max(ra, rb)
        return ['int', 'float', 'double'][r]

    # ---- calls

    def call_code(self, callee, name, args):
        if isinstance(callee, Proc):
            if callee.excluded:
                self.tr.warn(f'{self.proc.name} calls excluded {callee.name}')
                return (f'VB.NotPorted<{callee.rtype or "object"}>("{callee.name}")' if callee.is_function
                        else f'VB.NotPorted("{callee.name}")'), callee.rtype
            ptypes = []
            for p in callee.params:
                if p.is_array:
                    ptypes.append(('arr',))
                elif p.paramarray:
                    ptypes.append('params')
                else:
                    ptypes.append(vb_type(p.vtype))
            refs = callee.ref_params
            cs = callee.cs_name
            rtype = callee.rtype
        else:
            rtype, ptypes, refs = callee
            cs = self.tr.handwritten_names[name.lower()]
        parts = []
        for j, a in enumerate(args):
            pt = ptypes[j] if j < len(ptypes) else ptypes[-1] if ptypes and ptypes[-1] == 'params' else None
            if pt == 'params':
                c, t = self.ex(a)
                parts.append(conv(c, t, 'double'))
                continue
            if isinstance(pt, tuple):
                parts.append(self.expr(a))
                continue
            if j < len(refs) and refs[j]:
                parts.append(self.ref_arg(a, pt))
            else:
                parts.append(self.expr(a, pt))
        return f'{cs}({", ".join(parts)})', rtype

    def ref_arg(self, a, pt):
        if a.kind == 'name':
            v = self.tr.lookup_var(self.proc, a.name)
            if v is not None and not v.is_array and v.kind != 'const':
                if v.type != pt:
                    self.tr.warn(f'{self.proc.name}: ByRef type mismatch passing {a.name}')
                return f'ref {v.cs_name}'
        if a.kind == 'index':
            v = self.tr.lookup_var(self.proc, a.name)
            if v is not None and v.is_array:
                return f'ref {self.expr(a)}'
        # an expression: pass a temporary
        t = self.tmp()
        self.pre.append(f'{cs_type(pt)} {t} = {self.expr(a, pt)};')
        return f'ref {t}'

    # ---- statements

    def flush_pre(self):
        for p in self.pre:
            self.emit(p)
        self.pre = []

    def cond(self, e):
        code = self.expr(e, 'bool')
        return code

    def stmts(self, ss):
        for s in ss:
            self.stmt(s)

    def stmt(self, s):
        k = s.kind
        c = getattr(s, 'comment', None)
        if k == 'comment':
            self.emit('', s.text)
        elif k == 'dim':
            self.emit('', c) if c else None
        elif k == 'assign':
            self.assign(s, c)
        elif k == 'call':
            callee = self.tr.find_proc(self.proc.module, s.name)
            if callee is None:
                v = self.tr.lookup_var(self.proc, s.name)
                raise ValueError(f'{self.proc.name}: unknown procedure {s.name}' + (' (a variable)' if v else ''))
            code, _ = self.call_code(callee, s.name, s.args)
            self.flush_pre()
            if code.startswith('VB.NotPorted<'):
                code = code.replace(f'<{callee.rtype}>', '', 1) if isinstance(callee, Proc) else code
            self.emit(code + ';', c)
        elif k == 'exit':
            self.exit(s.what, c)
        elif k == 'erase':
            for n in s.names:
                v = self.tr.lookup_var(self.proc, n)
                if v is None or not v.is_array:
                    raise ValueError(f'{self.proc.name}: cannot Erase {n}')
                self.emit(f'{v.cs_name}.Clear();', c)
                c = None
        elif k == 'if':
            self.if_block(s)
        elif k == 'ifline':
            cond = self.cond(s.cond)
            self.flush_pre()
            self.emit(f'if ({cond})', c)
            self.braced(s.then)
            if s.els:
                self.emit('else')
                self.braced(s.els)
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
                self.emit('continue;', 'GoTo L1' + (f'  {c}' if c else ''))
            else:
                raise ValueError(f'{self.proc.name}: unsupported GoTo {s.label}')
        elif k == 'label':
            self.emit('', f'{s.name}:')
        elif k == 'end':
            self.emit('throw new KhHaltException("End");', c)
        else:
            raise ValueError(f'{self.proc.name}: unsupported statement {k}')

    def braced(self, ss):
        self.emit('{')
        self.indent += 1
        self.stmts(ss)
        self.indent -= 1
        self.emit('}')

    def assign(self, s, c):
        lhs = s.lhs
        if lhs.kind == 'member':
            code, t = self.ex_member(lhs)
            self.emit(f'{code} = {self.expr(s.rhs, t)};', c)
            return
        if lhs.kind == 'index' and lhs.name.lower() == 'mid':
            self.emit('', f'(not ported) Mid statement' + (f'  {c}' if c else ''))
            return
        if lhs.kind == 'name':
            v = self.tr.lookup_var(self.proc, lhs.name)
            if v is None:
                raise ValueError(f'{self.proc.name}: assignment to unknown {lhs.name}')
            target, t = v.cs_name, v.type
        elif lhs.kind == 'index':
            v = self.tr.lookup_var(self.proc, lhs.name)
            if v is None or not v.is_array:
                raise ValueError(f'{self.proc.name}: assignment to unknown array {lhs.name}')
            target, t = self.ex_index(lhs)
        else:
            raise ValueError(f'{self.proc.name}: bad assignment target')
        rhs = self.expr(s.rhs, t)
        self.flush_pre()
        self.emit(f'{target} = {rhs};', c)

    def exit(self, what, c):
        if what in ('sub', 'function'):
            self.emit('return __ret;' if self.proc.is_function else 'return;', c)
            return
        kind = {'for': 'for', 'do': 'do'}[what]
        for depth in range(len(self.loops) - 1, -1, -1):
            lk, entry = self.loops[depth]
            if lk == kind:
                if depth == len(self.loops) - 1:
                    self.emit('break;', c)
                else:
                    if entry['label'] is None:
                        self.tr.label += 1
                        entry['label'] = f'__exit{self.tr.label}'
                    self.emit(f'goto {entry["label"]};', c)
                return
        raise ValueError(f'{self.proc.name}: Exit {what} outside a loop')

    def if_block(self, s):
        first = True
        depth = 0
        comments = [s.comment] + list(getattr(s, 'clause_comments', []))
        for i, (cond, body) in enumerate(s.clauses):
            code = self.cond(cond)
            if self.pre:
                if first:
                    self.flush_pre()
                else:
                    # hoisted temporaries need a nested block
                    self.emit('else')
                    self.emit('{')
                    self.indent += 1
                    depth += 1
                    self.flush_pre()
                    first = True
            self.emit(('if' if first else 'else if') + f' ({code})', comments[i])
            self.braced(body)
            first = False
        if s.els is not None:
            self.emit('else', getattr(s, 'else_comment', None))
            self.braced(s.els)
        for _ in range(depth):
            self.indent -= 1
            self.emit('}')

    def select(self, s):
        t = self.tmp()
        code, ty = self.ex(s.e)
        self.flush_pre()
        self.emit(f'{cs_type(ty)} {t} = {code};', s.comment)
        first = True
        for items, body, comment in s.cases:
            conds = []
            for it in items:
                if it[0] == 'eq':
                    c, tc = self.ex(it[1])
                    tt = self.numeric(ty, tc) if ty != 'string' else 'string'
                    conds.append(f'{conv(t, ty, tt)} == {conv(c, tc, tt)}')
                elif it[0] == 'range':
                    conds.append(f'({t} >= {self.expr(it[1], ty)} && {t} <= {self.expr(it[2], ty)})')
                else:
                    sym = {'=': '==', '<>': '!='}.get(it[1], it[1])
                    conds.append(f'{t} {sym} {self.expr(it[2], ty)}')
            self.emit(('if' if first else 'else if') + f' ({" || ".join(conds)})', comment)
            self.braced(body)
            first = False
        if s.els is not None:
            if first:
                self.braced(s.els)
            else:
                self.emit('else', getattr(s, 'else_comment', None))
                self.braced(s.els)

    def loop_var(self, var):
        if var.kind != 'name':
            raise ValueError(f'{self.proc.name}: unsupported For variable')
        v = self.tr.lookup_var(self.proc, var.name)
        if v is None:
            raise ValueError(f'{self.proc.name}: unknown For variable {var.name}')
        return v.cs_name, v.type

    def for_loop(self, s):
        name, t = self.loop_var(s.var)
        start = self.expr(s.start, t)
        end = self.expr(s.end, t)
        self.flush_pre()
        e = self.tmp()
        step = None
        if s.step is None:
            step_code, sign = '1', 1
        elif s.step.kind == 'num' or (s.step.kind == 'unary' and s.step.e.kind == 'num'):
            val = s.step.v if s.step.kind == 'num' else -s.step.e.v
            step_code, sign = str(val), (1 if val >= 0 else -1)
        else:
            step_code, sign = self.tmp(), 0
            step = self.expr(s.step, t)
        entry = {'label': None}
        self.emit('{', s.comment)
        self.indent += 1
        self.emit(f'{cs_type(t)} {e} = {end};')
        if step is not None:
            self.emit(f'{cs_type(t)} {step_code} = {step};')
        if sign > 0:
            test = f'{name} <= {e}'
        elif sign < 0:
            test = f'{name} >= {e}'
        else:
            test = f'({step_code} >= 0 ? {name} <= {e} : {name} >= {e})'
        inc = f'{name}++' if step_code == '1' else (f'{name}--' if step_code == '-1' else f'{name} += {step_code}')
        self.emit(f'for ({name} = {start}; {test}; {inc})')
        self.loops.append(('for', entry))
        self.braced(s.body)
        self.loops.pop()
        self.indent -= 1
        self.emit('}')
        if entry['label']:
            self.emit(f'{entry["label"]}: ;')

    def do_loop(self, s):
        entry = {'label': None}
        self.loops.append(('do', entry))
        if s.how is not None:
            cond = self.cond(s.cond)
            if self.pre:
                raise ValueError(f'{self.proc.name}: hoisting in a loop condition')
            if s.how == 'until':
                cond = f'!{cond}'
            self.emit(f'while ({cond})', s.comment)
            self.braced(s.body)
        elif s.post_how is not None:
            self.emit('do', s.comment)
            self.braced(s.body)
            cond = self.cond(s.post_cond)
            if self.pre:
                raise ValueError(f'{self.proc.name}: hoisting in a loop condition')
            if s.post_how == 'until':
                cond = f'!{cond}'
            self.out[-1] += f' while ({cond});'
        else:
            self.emit('while (true)', s.comment)
            self.braced(s.body)
        self.loops.pop()
        if entry['label']:
            self.emit(f'{entry["label"]}: ;')

    # ---- procedure

    def generate(self):
        proc = self.proc
        node = proc.node
        out = []
        for lc in node.lead_comments:
            out.append(f'    // {lc}')
        params = []
        for i, p in enumerate(node.params):
            if p.paramarray:
                params.append(f'params double[] {cs_ident(p.name)}')
            elif p.is_array:
                params.append(f'{array_type(vb_type(p.vtype), None)} {cs_ident(p.name)}')
            else:
                params.append(('ref ' if proc.ref_params[i] else '') + f'{cs_type(vb_type(p.vtype))} {cs_ident(p.name)}')
        ret = cs_type(proc.rtype) if proc.is_function else 'void'
        sig = f'    {ret} {proc.cs_name}({", ".join(params)})'
        if node.comment:
            sig += f'  // {node.comment}'
        out.append(sig)
        out.append('    {')
        if proc.excluded:
            out.append(f'        throw new NotSupportedException("{proc.name} is not ported");')
            out.append('    }')
            return out, []
        self.indent = 2
        fields = []
        # locals: static locals become fields; others are C# locals initialized like VB's
        for v in proc.locals.values():
            if v.kind == 'const':
                fields.insert(0, f'    const {cs_type(v.ctype)} {v.cs_name} = {self.expr(v.const_value, v.ctype)};')
            elif v.kind in ('static', 'local'):
                if v.kind == 'static':
                    fields.append(field_decl(self.tr, v, proc))
                else:
                    self.emit(local_decl(self.tr, v, proc))
        if proc.is_function:
            self.emit(f'{cs_type(proc.rtype)} __ret = {default_value(proc.rtype)};')
        self.stmts(node.body)
        if proc.is_function:
            self.emit('return __ret;')
        out.extend(self.out)
        out.append('    }')
        return out, fields


def cs_type(t):
    if isinstance(t, tuple):
        return array_type(t[1], t[2])
    return {'int': 'int', 'bool': 'bool', 'float': 'float', 'double': 'double', 'string': 'string',
            'object': 'object', None: 'object'}[t]


def array_type(elem, ndims):
    return f'VbArray<{cs_type(elem)}>'


def default_value(t):
    return {'int': '0', 'bool': 'false', 'float': '0f', 'double': '0.0', 'string': '""', 'object': 'null'}[t]


def bounds_code(tr, v, proc):
    gen = ProcGen(tr, proc) if proc else ModuleGen(tr)
    parts = []
    for lo, hi in v.dims:
        parts.append(gen.expr(lo, 'int'))
        parts.append(gen.expr(hi, 'int'))
    return ', '.join(parts)


def field_decl(tr, v, proc):
    if v.is_array:
        return f'    {array_type(v.ctype, len(v.dims))} {v.cs_name} = new({bounds_code(tr, v, proc)});'
    return f'    {cs_type(v.ctype)} {v.cs_name} = {default_value(v.ctype)};'


def local_decl(tr, v, proc):
    if v.is_array:
        return f'{array_type(v.ctype, len(v.dims))} {v.cs_name} = new({bounds_code(tr, v, proc)});'
    return f'{cs_type(v.ctype)} {v.cs_name} = {default_value(v.ctype)};'


class ModuleGen(ProcGen):
    """Expression generation at module level (for constants and array bounds)."""

    def __init__(self, tr):
        class Dummy:
            name = '(module)'
            module = 'Globals'
            locals = {}
            is_function = False
        super().__init__(tr, Dummy())


def module_fields(tr, mod):
    lines = []
    for d in mod.decls:
        for dv in d.decls:
            low = dv.name.lower()
            v = tr.globals.get(low) if (d.scope in ('public', 'global') or mod.name == 'Globals') else tr.module_vars.get((mod.name, low))
            comment = f'  // {d.comment}' if d.comment and dv is d.decls[-1] else ''
            if v.kind == 'const':
                gen = ModuleGen(tr)
                gen.proc.module = mod.name
                lines.append(f'    const {cs_type(v.ctype)} {v.cs_name} = {gen.expr(dv.value, v.ctype)};{comment}')
            elif v.is_array:
                gen = ModuleGen(tr)
                gen.proc.module = mod.name
                parts = []
                for lo, hi in v.dims:
                    parts.append(gen.expr(lo, 'int'))
                    parts.append(gen.expr(hi, 'int'))
                lines.append(f'    {array_type(v.ctype, len(v.dims))} {v.cs_name} = new({", ".join(parts)});{comment}')
            else:
                lines.append(f'    {cs_type(v.ctype)} {v.cs_name} = {default_value(v.ctype)};{comment}')
    return lines


def main():
    src, outdir = sys.argv[1], sys.argv[2]
    modules = [read_module(os.path.join(src, f), name, excl, incl) for f, name, excl, incl in SOURCES]
    tr = Translator(modules)
    os.makedirs(outdir, exist_ok=True)
    for mod in modules:
        lines = [
            '// <auto-generated>',
            f'// Translated from Killer Hearts\' {[f for f, n, _, _ in SOURCES if n == mod.name][0]} by Transpiler/vb2cs.py.',
            '// Do not edit: change the translator and regenerate instead.',
            '// </auto-generated>',
            '',
            '#pragma warning disable CS0162, CS0168, CS0219, CS0414, CS0649, CS1717, IDE0059',
            '',
            'namespace KHearts;',
            '',
            'partial class KhEngine',
            '{',
        ]
        for hc in mod.header_comments:
            lines.append(f'    // {hc}')
        lines.extend(module_fields(tr, mod))
        proc_fields, proc_lines = [], []
        for p in mod.procs:
            proc = tr.procs[(mod.name, p.name.lower())]
            gen = ProcGen(tr, proc)
            body, fields = gen.generate()
            proc_lines.append('')
            proc_lines.extend(body)
            if fields:
                proc_fields.append(f'    // Static locals of {proc.name}')
                proc_fields.extend(fields)
        if proc_fields:
            lines.append('')
            lines.extend(proc_fields)
        lines.extend(proc_lines)
        if mod.name == 'Form':
            lines.append('')
            lines.append('    // Stand-ins for form controls the engine reads')
            for f, t in sorted(tr.ui_fields.items()):
                lines.append(f'    {cs_type(t)} {f} = {default_value(t)};')
        lines.append('}')
        with open(os.path.join(outdir, f'{mod.name}.g.cs'), 'w', encoding='utf-8', newline='\n') as f:
            f.write('\n'.join(lines) + '\n')
    for w in sorted(set(tr.warnings)):
        print('warning:', w, file=sys.stderr)


if __name__ == '__main__':
    main()
