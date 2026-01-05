import { Injectable } from '@angular/core';

export interface AppConfig {
    apiUrl: string;
    production: boolean;
}

@Injectable({
    providedIn: 'root'
})
export class ConfigService {
    private config: AppConfig | null = null;

    /**
     * Load configuration from config.json file
     * This must be called before the app is bootstrapped
     */
    async loadConfig(): Promise<AppConfig> {
        if (this.config) {
            return this.config;
        }

        try {
            const response = await fetch('/config.json');
            if (!response.ok) {
                throw new Error(`Failed to load config: ${response.statusText}`);
            }
            this.config = await response.json();
            console.log('Configuration loaded:', this.config);
            return this.config!; // Non-null assertion since we just assigned it
        } catch (error) {
            console.error('Error loading configuration, using defaults:', error);
            // Fallback to localhost if config.json doesn't exist
            this.config = {
                apiUrl: 'http://localhost:5174/api',
                production: false
            };
            return this.config;
        }
    }

    /**
     * Get the current configuration
     * Throws error if config hasn't been loaded yet
     */
    getConfig(): AppConfig {
        if (!this.config) {
            throw new Error('Configuration not loaded! Call loadConfig() first.');
        }
        return this.config;
    }

    /**
     * Get API URL from configuration
     */
    get apiUrl(): string {
        return this.getConfig().apiUrl;
    }

    /**
     * Check if running in production mode
     */
    get isProduction(): boolean {
        return this.getConfig().production;
    }
}
