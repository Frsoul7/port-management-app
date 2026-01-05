using System;
using System.Linq;
using System.Text.RegularExpressions;

namespace DDDNetCore.Domain.Shared
{
    public static class Iso6346
    {
        // 4 letters (owner + category), 6 digits (serial), 1 digit (check)
        // Category letter must be U, J or Z
        private static readonly Regex Pattern = new(@"^[A-Z]{3}[UJZ][0-9]{6}[0-9]$",
            RegexOptions.Compiled | RegexOptions.CultureInvariant);

        public static bool IsValid(string code)
        {
            if (string.IsNullOrWhiteSpace(code)) return false;

            code = code.Trim().ToUpperInvariant();
            if (!Pattern.IsMatch(code)) return false;

            // check digit verification
            var expected = ComputeCheckDigit(code);
            var provided = code[^1] - '0';
            return expected == provided;
        }

        public static int ComputeCheckDigit(string code)
        {
            // Assumes already UPPER + matches pattern
            // Use first 10 chars (exclude the check digit itself)
            int sum = 0;
            for (int pos = 0; pos < 10; pos++)
            {
                int v = CharValue(code[pos]);
                int weight = 1 << pos; // 2^pos
                sum += v * weight;
            }

            int remainder = sum % 11;
            return remainder == 10 ? 0 : remainder;
        }

        // ISO 6346 letter values (A=10, B=12, C=13, ... Z=38; 11,22,33 skipped)
        private static int CharValue(char c)
        {
            if (c >= '0' && c <= '9') return c - '0';

            // Map A..Z to {10,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38}
            int alphaIndex = c - 'A';         // 0..25
            int value = 10 + alphaIndex;      // 10..35
            // Skip multiples of 11 (11,22,33) by adding +1 each time we pass them
            // A=10 (ok), B=11->12, C=13, ..., L would hit 21->(ok), M=22->24, V hits 31->(ok), W=32->34, etc.
            if (value >= 11) value++;
            if (value >= 22) value++;
            if (value >= 33) value++;
            return value;
        }
    }
}
