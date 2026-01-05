/**
 * Prettier Configuration for OEM Backend
 * 
 * Prettier is an opinionated code formatter that ensures
 * consistent code style across the entire project.
 * 
 * Usage:
 *   npm run format        - Format all files
 *   npm run format:check  - Check formatting without changing files
 */
module.exports = {
  semi: true,              // Add semicolons at end of statements
  trailingComma: 'es5',   // Add trailing commas where valid in ES5 (objects, arrays)
  singleQuote: true,      // Use single quotes instead of double quotes
  printWidth: 100,        // Wrap lines at 100 characters
  tabWidth: 2,            // Use 2 spaces for indentation
  useTabs: false,         // Use spaces, not tabs
  arrowParens: 'always',  // Always include parentheses around arrow function parameters
  endOfLine: 'lf',        // Use Unix line endings (\n) instead of Windows (\r\n)
};
