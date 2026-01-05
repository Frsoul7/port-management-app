/**
 * VVE ID Generator Service
 * Generates sequential VVE identifiers: VVE-000001, VVE-000002, etc.
 * US 4.1.7: VVE identifier pattern similar to VVN IDs
 */
export class VveIdGenerator {
  /**
   * Generate VVE ID from sequential number
   * @param sequentialNumber - The next sequential number (1, 2, 3, ...)
   * @returns VVE ID in format VVE-XXXXXX (6 digits, zero-padded)
   */
  static generate(sequentialNumber: number): string {
    const paddedNumber = sequentialNumber.toString().padStart(6, '0');
    return `VVE-${paddedNumber}`;
  }

  /**
   * Extract sequential number from VVE ID
   * @param vveId - VVE ID (e.g., "VVE-000001")
   * @returns Sequential number (e.g., 1)
   */
  static extractSequentialNumber(vveId: string): number {
    const match = vveId.match(/^VVE-(\d{6})$/);
    if (!match) {
      throw new Error(`Invalid VVE ID format: ${vveId}`);
    }
    return parseInt(match[1]!, 10);
  }

  /**
   * Validate VVE ID format
   * @param vveId - VVE ID to validate
   * @returns true if valid format
   */
  static isValid(vveId: string): boolean {
    return /^VVE-\d{6}$/.test(vveId);
  }
}
