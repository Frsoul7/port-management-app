using System.Text.RegularExpressions;

namespace DDDNetCore.Application.Validators
{
    /// <summary>
    /// Provides email validation functionality with a realistic pattern.
    /// Pattern accepts: something@something.something
    /// Examples: user@example.com, john.doe@company.co.uk, test+tag@domain.org
    /// </summary>
    public static class EmailValidator
    {
        /// <summary>
        /// Regex pattern for email validation.
        /// Accepts: local-part@domain.tld format
        /// - Local part: alphanumeric, dots, hyphens, underscores, plus signs
        /// - Domain: alphanumeric, dots, hyphens
        /// - TLD: at least 2 letters
        /// </summary>
        public const string EmailPattern = @"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$";

        private static readonly Regex EmailRegex = new Regex(EmailPattern, RegexOptions.Compiled | RegexOptions.IgnoreCase);

        /// <summary>
        /// Validates if the provided email address matches the expected format.
        /// </summary>
        /// <param name="email">The email address to validate</param>
        /// <returns>True if valid, false otherwise</returns>
        public static bool IsValid(string? email)
        {
            if (string.IsNullOrWhiteSpace(email))
                return false;

            var trimmedEmail = email.Trim();

            // Basic checks
            if (trimmedEmail.Length < 5) // minimum: a@b.c
                return false;

            if (trimmedEmail.Length > 254) // RFC 5321
                return false;

            // Regex validation
            return EmailRegex.IsMatch(trimmedEmail);
        }

        /// <summary>
        /// Validates an email address and throws an ArgumentException if invalid.
        /// </summary>
        /// <param name="email">The email address to validate</param>
        /// <param name="paramName">The parameter name for the exception</param>
        /// <exception cref="ArgumentException">Thrown when email is invalid</exception>
        public static void ValidateAndThrow(string? email, string paramName = "email")
        {
            if (!IsValid(email))
            {
                throw new ArgumentException($"Email must be a valid email address (format: something@something.something). Invalid: {email}", paramName);
            }
        }
    }
}
