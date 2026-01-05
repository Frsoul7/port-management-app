/**
 * Environment Configuration
 * MUST be imported FIRST before any other modules
 * Loads .env file and makes variables available via process.env
 */
import dotenv from 'dotenv';
import path from 'path';

// Load .env from backend-oem root directory
const envPath = path.join(process.cwd(), '.env');
const result = dotenv.config({ path: envPath });

if (result.error) {
  console.error('❌ Error loading .env:', result.error.message);
  process.exit(1);
}

console.log('✅ Environment loaded from:', envPath);
console.log('✅ CORE_BACKEND_URL:', process.env.CORE_BACKEND_URL);
console.log('✅ JWT_SECRET length:', process.env.JWT_SECRET?.length);
