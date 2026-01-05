import { bootstrapApplication } from '@angular/platform-browser';
import { appConfig } from './app/app.config';
import { AppComponent } from './app/app.component';
import { ConfigService } from './app/core/services/config.service';

// Load configuration before bootstrapping the app
const configService = new ConfigService();
configService.loadConfig()
  .then(() => {
    console.log('Configuration loaded, bootstrapping app...');
    return bootstrapApplication(AppComponent, appConfig);
  })
  .catch((err) => console.error('Failed to bootstrap app:', err));
