import { Injectable, inject, signal, computed } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, BehaviorSubject } from 'rxjs';
import { tap } from 'rxjs/operators';

/**
 * Translation service for multilingual support (US 3.1.2)
 * Simple implementation without external library for now
 * TODO: Can be replaced with @ngx-translate/core if needed
 */
@Injectable({
  providedIn: 'root'
})
export class TranslationService {
  private http = inject(HttpClient);
  private currentLangSignal = signal<string>('en');
  private translationsSignal = signal<any>({});

  public readonly currentLang = this.currentLangSignal.asReadonly();
  public readonly translations = this.translationsSignal.asReadonly();

  constructor() {
    // Load default language from localStorage or use 'en'
    const savedLang = (localStorage.getItem('language') || 'en') as 'en' | 'pt';
    this.setLanguage(savedLang);
  }

  /**
   * Set current language and load translations
   */
  setLanguage(lang: 'en' | 'pt'): void {
    console.log(`TranslationService: Setting language to ${lang}`);
    this.currentLangSignal.set(lang);
    localStorage.setItem('language', lang);
    
    // Load translations from JSON file
    this.loadTranslations(lang).subscribe({
      next: (translations) => {
        console.log(`TranslationService: Loaded translations for ${lang}:`, translations);
        this.translationsSignal.set(translations);
      },
      error: (error) => {
        console.error(`Failed to load translations for ${lang}:`, error);
        // Keep existing translations if load fails
      }
    });
  }

  /**
   * Get translation for a key (e.g., 'APP.TITLE')
   */
  translate(key: string): string {
    const translations = this.translationsSignal();
    
    // If translations are empty, return the key
    if (!translations || Object.keys(translations).length === 0) {
      return key;
    }
    
    const keys = key.split('.');
    
    let value: any = translations;
    for (const k of keys) {
      value = value?.[k];
      if (value === undefined) break;
    }
    
    return value || key;
  }

  /**
   * Load translations from JSON file
   */
  private loadTranslations(lang: string): Observable<any> {
    return this.http.get(`/i18n/${lang}.json`);
  }

  /**
   * Toggle between English and Portuguese
   */
  toggleLanguage(): void {
    const newLang = this.currentLang() === 'en' ? 'pt' : 'en';
    this.setLanguage(newLang);
  }
}
