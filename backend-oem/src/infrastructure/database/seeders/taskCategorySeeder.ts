/**
 * Task Category Seeder
 * Seeds initial task categories for development/testing environments
 * US 4.1.14: Complementary task categories catalog
 */

import { logger } from '@shared/utils/logger';
import { TaskCategoryRepository } from '../../repositories/TaskCategoryRepository';
import { TaskCategory } from '@domain/entities/TaskCategory';

/**
 * Seed Task Categories for development/testing
 * Creates example categories from US 4.1.14 acceptance criteria
 */
export async function seedTaskCategoriesIfNeeded(): Promise<void> {
  try {
    const taskCategoryRepository = new TaskCategoryRepository();

    // Check if any task categories already exist
    const existingCategories = await taskCategoryRepository.findAll();
    if (existingCategories.length > 0) {
      logger.debug(`Task categories already seeded (${existingCategories.length} categories found)`);
      return;
    }

    logger.info('Seeding task categories...');

    // US 4.1.14: Example categories from acceptance criteria
    const seedCategories = [
      // Safety and Security
      new TaskCategory({
        categoryCode: 'CTC001',
        categoryName: 'Onboard Security Check',
        description: 'Security inspection performed by port authority to ensure vessel compliance with security protocols',
        defaultDurationHours: 1.5,
        expectedImpact: 'May delay cargo operations by 1-2 hours',
      }),
      new TaskCategory({
        categoryCode: 'CTC002',
        categoryName: 'Customs Inspection',
        description: 'Customs authority inspection of vessel cargo and documentation',
        defaultDurationHours: 2,
        expectedImpact: 'Cargo operations suspended during inspection',
      }),

      // Maintenance
      new TaskCategory({
        categoryCode: 'CTC003',
        categoryName: 'Hull Repair',
        description: 'Minor hull maintenance and repairs performed during vessel visit',
        defaultDurationHours: 4,
        expectedImpact: 'Vessel must remain stationary, cargo operations may continue',
      }),
      new TaskCategory({
        categoryCode: 'CTC004',
        categoryName: 'Equipment Calibration',
        description: 'Calibration and testing of cargo handling equipment',
        defaultDurationHours: 1,
        expectedImpact: 'Affected equipment unavailable during calibration',
      }),

      // Cleaning and Housekeeping
      new TaskCategory({
        categoryCode: 'CTC005',
        categoryName: 'Deck Cleaning',
        description: 'Cleaning and sanitation of vessel deck areas',
        defaultDurationHours: 2,
        expectedImpact: 'May run in parallel with cargo operations',
      }),
      new TaskCategory({
        categoryCode: 'CTC006',
        categoryName: 'Waste Removal',
        description: 'Collection and disposal of vessel waste and recyclables',
        defaultDurationHours: 0.5,
        expectedImpact: 'Minimal impact, runs in parallel',
      }),

      // Additional common categories
      new TaskCategory({
        categoryCode: 'CTC007',
        categoryName: 'Fuel Bunkering',
        description: 'Refueling operations for vessel',
        defaultDurationHours: 3,
        expectedImpact: 'Cargo operations may be suspended for safety',
      }),
      new TaskCategory({
        categoryCode: 'CTC008',
        categoryName: 'Safety Drill',
        description: 'Mandatory safety drill and emergency procedure verification',
        defaultDurationHours: 1,
        expectedImpact: 'All operations temporarily suspended',
      }),
    ];

    // Save all categories
    for (const category of seedCategories) {
      await taskCategoryRepository.save(category);
      logger.info(`  ✅ Created task category ${category.categoryCode}: ${category.categoryName}`);
    }

    logger.info(`✅ Seeded ${seedCategories.length} task categories successfully`);
  } catch (error) {
    logger.warn('Could not seed task categories:', error);
    // Don't fail the app startup if seeding fails
  }
}
