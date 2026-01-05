/**
 * Database Seeders Index
 * 
 * Centralized export for all database seeders.
 * Seeders are used to populate initial data for development/testing environments.
 */

export { seedVveFromOperationPlanIfNeeded } from './vveSeeder';
export { seedTaskCategoriesIfNeeded } from './taskCategorySeeder';

/**
 * Run all seeders
 * Call this function to seed all necessary data for development
 */
export async function runAllSeeders(): Promise<void> {
  const { seedTaskCategoriesIfNeeded } = await import('./taskCategorySeeder');
  const { seedVveFromOperationPlanIfNeeded } = await import('./vveSeeder');
  
  await seedTaskCategoriesIfNeeded();
  await seedVveFromOperationPlanIfNeeded();
}
