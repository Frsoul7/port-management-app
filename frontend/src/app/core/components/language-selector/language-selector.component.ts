import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TranslationService } from '../../services/translation.service';

@Component({
  selector: 'app-language-selector',
  standalone: true,
  imports: [CommonModule],
  template: `
    <div class="language-selector">
      <button 
        class="lang-btn"
        [class.active]="currentLang() === 'en'"
        (click)="setLanguage('en')"
        title="English">
        🇬🇧 EN
      </button>
      <button 
        class="lang-btn"
        [class.active]="currentLang() === 'pt'"
        (click)="setLanguage('pt')"
        title="Português">
        🇵🇹 PT
      </button>
    </div>
  `,
  styles: [`
    .language-selector {
      display: flex;
      gap: 4px;
      align-items: center;
      width: 100%;
    }

    .lang-btn {
      background: transparent;
      border: 1px solid rgba(255, 255, 255, 0.3);
      color: white;
      padding: 6px 10px;
      border-radius: 4px;
      cursor: pointer;
      font-size: 12px;
      font-weight: 500;
      transition: all 0.2s;
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 4px;
      white-space: nowrap;
      flex: 1;
    }

    .lang-btn:hover {
      background: rgba(255, 255, 255, 0.1);
      border-color: rgba(255, 255, 255, 0.5);
      transform: scale(1.02);
    }

    .lang-btn.active {
      background: rgba(255, 255, 255, 0.2);
      border-color: rgba(255, 255, 255, 0.6);
      font-weight: 600;
    }

    @media (max-width: 768px) {
      .lang-btn {
        padding: 4px 8px;
        font-size: 12px;
        flex: 0 0 auto;
      }
    }
  `]
})
export class LanguageSelectorComponent {
  constructor(private translationService: TranslationService) {}

  get currentLang() {
    return this.translationService.currentLang;
  }

  setLanguage(lang: 'en' | 'pt'): void {
    this.translationService.setLanguage(lang);
  }
}
