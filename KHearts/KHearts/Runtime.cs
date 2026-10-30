using System.Globalization;

namespace KHearts;

/// <summary>
/// A VB6 array: one to four dimensions, each with its own lower and upper bound. Like VB6, it checks
/// every subscript against its own dimension's bounds. Elements are returned by reference, so they can
/// be assigned and passed ByRef.
/// </summary>
public sealed class VbArray<T>
{
    readonly int[] lower;
    readonly int[] length;
    readonly T[] data;

    /// <summary>Creates an array from (lower, upper) bound pairs, one pair per dimension.</summary>
    public VbArray(params int[] bounds)
    {
        if (bounds.Length == 0 || bounds.Length % 2 != 0)
            throw new ArgumentException("Bounds must come in pairs");
        var rank = bounds.Length / 2;
        lower = new int[rank];
        length = new int[rank];
        var size = 1;
        for (var d = 0; d < rank; d++)
        {
            lower[d] = bounds[2 * d];
            length[d] = bounds[2 * d + 1] - bounds[2 * d] + 1;
            size *= length[d];
        }
        data = new T[size];
    }

    public int Rank => lower.Length;

    public int LowerBound(int dimension) => lower[dimension];

    public int UpperBound(int dimension) => lower[dimension] + length[dimension] - 1;

    int Offset(int subscript, int dimension)
    {
        var k = subscript - lower[dimension];
        if ((uint)k >= (uint)length[dimension])
            throw new IndexOutOfRangeException(
                $"Subscript out of range: {subscript} not in {lower[dimension]} To {UpperBound(dimension)}");
        return k;
    }

    void CheckRank(int rank)
    {
        if (rank != lower.Length)
            throw new IndexOutOfRangeException($"Array has {lower.Length} dimensions, not {rank}");
    }

    public ref T this[int i]
    {
        get { CheckRank(1); return ref data[Offset(i, 0)]; }
    }

    public ref T this[int i, int j]
    {
        get { CheckRank(2); return ref data[Offset(i, 0) * length[1] + Offset(j, 1)]; }
    }

    public ref T this[int i, int j, int k]
    {
        get
        {
            CheckRank(3);
            return ref data[(Offset(i, 0) * length[1] + Offset(j, 1)) * length[2] + Offset(k, 2)];
        }
    }

    public ref T this[int i, int j, int k, int l]
    {
        get
        {
            CheckRank(4);
            return ref data[((Offset(i, 0) * length[1] + Offset(j, 1)) * length[2] + Offset(k, 2)) * length[3]
                + Offset(l, 3)];
        }
    }

    /// <summary>VB6's Erase: resets every element to its default.</summary>
    public void Clear() => Array.Clear(data);

    /// <summary>Copies another array's elements, which must have the same shape.</summary>
    public void CopyFrom(VbArray<T> other) => Array.Copy(other.data, data, data.Length);
}

/// <summary>Killer Hearts halted, as VB6's diagnose_and_halt or End would.</summary>
public sealed class KhHaltException(string message) : Exception(message);

/// <summary>VB6 conversions and functions, with VB6's semantics.</summary>
static class VB
{
    /// <summary>VB6's True is -1.</summary>
    public static int B2I(bool b) => b ? -1 : 0;

    /// <summary>CLng: rounds half to even, and overflows loudly.</summary>
    public static int CLng(double d) => checked((int)Math.Round(d, MidpointRounding.ToEven));

    public static int CLng(float f) => CLng((double)f);

    public static double Int(double d) => Math.Floor(d);

    public static double Fix(double d) => Math.Truncate(d);

    public static string CStr(int i) => i.ToString(CultureInfo.InvariantCulture);

    public static string CStr(bool b) => b ? "True" : "False";

    public static string CStr(float f) => f.ToString(CultureInfo.InvariantCulture);

    public static string CStr(double d) => d.ToString(CultureInfo.InvariantCulture);

    public static string CStr(string s) => s;

    public static bool CBool(string s) => s.Equals("True", StringComparison.OrdinalIgnoreCase) || Val(s) != 0;

    public static double Val(string s) =>
        double.TryParse(s.Trim(), NumberStyles.Float, CultureInfo.InvariantCulture, out var d) ? d : 0;

    public static int Len(string s) => s.Length;

    /// <summary>Mid(s, start[, length]), 1-based.</summary>
    public static string Mid(string s, int start, int length = int.MaxValue)
    {
        if (start < 1)
            throw new ArgumentException("Invalid procedure call: Mid start < 1");
        if (start > s.Length)
            return "";
        return s.Substring(start - 1, Math.Min(length, s.Length - start + 1));
    }

    public static string Left(string s, int length) => s[..Math.Min(length, s.Length)];

    public static string Right(string s, int length) => s[Math.Max(0, s.Length - length)..];

    /// <summary>Called where the original calls a skill level other than best, which is not ported.</summary>
    public static void NotPorted(string name) =>
        throw new NotSupportedException($"{name} is not ported: only the best player is");

    public static T NotPorted<T>(string name) =>
        throw new NotSupportedException($"{name} is not ported: only the best player is");
}
