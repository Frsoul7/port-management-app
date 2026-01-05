/**
 * Utility functions
 */

/**
 * Convert DateTime to hours since midnight
 * @param dateTime ISO 8601 datetime string
 * @returns Hours since midnight (e.g., 8.5 for 08:30)
 */
export function dateTimeToHours(dateTime: string | Date): number {
  const date = new Date(dateTime);
  return date.getHours() + date.getMinutes() / 60;
}

/**
 * Convert hours since midnight to DateTime
 * @param baseDate Base date (YYYY-MM-DD)
 * @param hours Hours since midnight (e.g., 8.5 for 08:30)
 * @returns ISO 8601 datetime string
 */
export function hoursToDateTime(baseDate: string, hours: number): Date {
  const date = new Date(baseDate);
  const totalMinutes = Math.round(hours * 60);
  date.setHours(0, totalMinutes, 0, 0);
  return date;
}

/**
 * Validate if date string is valid
 * @param dateStr Date string
 * @returns true if valid
 */
export function isValidDate(dateStr: string): boolean {
  const date = new Date(dateStr);
  return date instanceof Date && !isNaN(date.getTime());
}

/**
 * Format date to YYYY-MM-DD
 * @param date Date object
 * @returns Formatted date string
 */
export function formatDate(date: Date): string {
  return date.toISOString().split('T')[0] || '';
}

/**
 * Check if date is in the past
 * @param date Date to check
 * @returns true if in the past
 */
export function isPastDate(date: Date): boolean {
  const today = new Date();
  today.setHours(0, 0, 0, 0);
  return date < today;
}

/**
 * Calculate duration in hours between two dates
 * @param start Start date
 * @param end End date
 * @returns Duration in hours
 */
export function calculateDurationHours(start: Date, end: Date): number {
  const diffMs = end.getTime() - start.getTime();
  return diffMs / (1000 * 60 * 60);
}

/**
 * Round number to specified decimal places
 * @param value Number to round
 * @param decimals Number of decimal places
 * @returns Rounded number
 */
export function roundToDecimals(value: number, decimals: number = 2): number {
  const factor = Math.pow(10, decimals);
  return Math.round(value * factor) / factor;
}

/**
 * Generate UUID v4
 * @returns UUID string
 */
export function generateUUID(): string {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Sleep for specified milliseconds (useful for testing)
 * @param ms Milliseconds to sleep
 */
export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
