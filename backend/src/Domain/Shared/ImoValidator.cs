namespace DDDNetCore.Domain.Shared

{
    public static class ImoValidator
    {
        /// <summary>
        /// Validates an IMO number: 7 digits, last is mod-10 check digit.
        /// </summary>
        public static bool IsValid(string imo)
        {
            if (string.IsNullOrWhiteSpace(imo)) return false;

            var s = imo.Trim();
            if (s.Length != 7 || !s.All(char.IsDigit)) return false;

            int d1 = s[0] - '0', d2 = s[1] - '0', d3 = s[2] - '0', d4 = s[3] - '0',
                d5 = s[4] - '0', d6 = s[5] - '0', d7 = s[6] - '0';

            int sum = d1 * 7 + d2 * 6 + d3 * 5 + d4 * 4 + d5 * 3 + d6 * 2;
            int check = sum % 10;
            return check == d7;
        }

        /// <summary>Optional helper to trim/normalize input.</summary>
        public static string Normalize(string imo) => (imo ?? string.Empty).Trim();
    }
}
