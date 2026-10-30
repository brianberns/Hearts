namespace KHearts;

/// <summary>
/// The routines of Killer Hearts' Dk_hndrl.bas that the engine uses, ported by hand. Those that only
/// drive the user interface (the "thinking" billboard, sounds, messages) do nothing here.
/// </summary>
partial class KhEngine
{
    int get_next_player(int arg) => (arg + 1) % 4;

    int get_previous_player(int arg) => (arg + 3) % 4;

    void get_index_from_rank_and_suit(ref int i, int j, int k)
    {
        if (j >= 2 && j <= ace && k >= spades && k <= diamonds)
            i = 4 * (j - 2) + k;                        // i = 0-51 = index to card_face
        else
            diagnose_and_halt("Illegal card rank or suit");
    }

    void get_rank_and_suit_from_index(int i, ref int j, ref int k)
    {
        if (i < 0 || i > 51)
            diagnose_and_halt("Card number outside range 0-51: " + i);
        k = i % 4;
        j = i / 4 + 2;                                  // rank is 2 - 14
    }

    // mini, maxi, minr, maxr take Variants, compare them as given, and convert only on assignment
    int mini(params double[] args)
    {
        var result = 32767;
        foreach (var arg in args)
            if (arg < result)
                result = VB.CLng(arg);
        return result;
    }

    int maxi(params double[] args)
    {
        var result = -32767;
        foreach (var arg in args)
            if (arg > result)
                result = VB.CLng(arg);
        return result;
    }

    float minr(params double[] args)
    {
        var result = 1E+28f;
        foreach (var arg in args)
            if (arg < result)
                result = (float)arg;
        return result;
    }

    float maxr(params double[] args)
    {
        var result = -1E+28f;
        foreach (var arg in args)
            if (arg > result)
                result = (float)arg;
        return result;
    }

    int count_bits_on(int arg)
    {
        var count = 0;                                  // if arg = 0, then count_bits_on = 0
        if (arg > 0)
        {
            for (var i = 2; i <= ace; i++)
                if ((arg & bit_no_on[i - 2]) > 0)
                    count++;
        }
        else if (arg < 0)
            diagnose_and_halt("Function count_bits_on has negative argument");
        return count;
    }

    string bit_numbers_on(int arg)
    {
        if (arg < 0 || arg > 1073741824)
            return "error";
        if (arg == 0)
            return "none";
        var result = "";
        var j = arg;
        var k = 0;                                      // count of bits on
        while (j > 0)
        {
            int m;
            for (m = 0; m <= 30; m++)
                if (bit_no_on[m] >= j)
                    break;
            if (m > 30 || bit_no_on[m] > j)
                m--;
            j -= bit_no_on[m];
            k++;
            if (k > 1)
                result += " / ";
            result += m;
        }
        return result;
    }

    // Formatting for the billboard only: these never affect play
    string format_I(int arg, int W)
    {
        var s = VB.CStr(arg);
        if ((arg < 0 && s.Length > W) || (arg >= 0 && s.Length > W - 1))
            return "*";
        return s.PadLeft(W);
    }

    string format_F(float arg, int W, int d) =>
        d < 0 || d >= W - 2 || W > 15 ? "*" : arg.ToString("F" + d, System.Globalization.CultureInfo.InvariantCulture).PadLeft(W);

    string name_string(VbArray<bool> array_name)
    {
        var result = "none";
        for (var j = player_west; j <= player_south; j++)
            if (array_name[j] && j != player)
                result += player_name[j] + ", ";
        if (result != "none")
            result = VB.Mid(result, 5, result.Length - 6);
        return result;
    }

    // input_array is the score array (by player); output_array gets its elements in order, low to high (1 to 4)
    void indirect_sort(VbArray<int> input_array, VbArray<int> output_array)
    {
        var value_array = new VbArray<int>(0, 125);
        output_array.Clear();
        for (var p = player_west; p <= player_south; p++)
            value_array[input_array[p]]++;
        var q = 0;
        for (var i = 0; i <= 125; i++)
        {
            while (value_array[i] > 0)
            {
                q++;
                output_array[q] = i;
                value_array[i]--;
            }
            if (q == 4)
                return;
        }
        diagnose_and_halt("Sub indirect_sort cannot find four values");
    }

    int truth_map(bool p, int true_value, int false_value) => p ? true_value : false_value;

    void bb_add(string s) { }

    void bb_cla(string s) { }

    void display_center_message(string msg, int color) { }

    void capture_snapshot(string message) { }

    void print_instantaneous_data() { }

    void PlaySoundX(string sound_file) { }

    void turn_off_sound() { }

    void diagnose_and_halt(string error_message) =>
        throw new KhHaltException(error_message);
}
