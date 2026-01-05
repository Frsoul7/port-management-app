-- Seed initial Privacy Policy for GDPR compliance (US 4.5.1 / US 4.5.2)
-- This creates the initial privacy policy that users will need to acknowledge

DECLARE @PolicyId UNIQUEIDENTIFIER = NEWID();
DECLARE @CreatedBy UNIQUEIDENTIFIER = '00000000-0000-0000-0000-000000000001'; -- System user

-- Insert Portuguese version
INSERT INTO PrivacyPolicies (PolicyId, Version, Content, ChangeSummary, EffectiveDate, CreatedBy, CreatedAt, IsActive, LanguageCode)
VALUES (
    @PolicyId,
    '1.0',
    N'# Política de Privacidade

## 1. Introdução

O Sistema de Gestão Portuária ("Sistema") está comprometido em proteger a sua privacidade e dados pessoais em conformidade com o Regulamento Geral sobre a Proteção de Dados (RGPD) da União Europeia.

## 2. Dados Recolhidos

Recolhemos os seguintes tipos de dados pessoais:
- **Dados de identificação**: Nome, email, identificador de utilizador
- **Dados profissionais**: Organização, cargo, número mecanográfico
- **Dados de acesso**: Logs de autenticação, endereço IP, informações do navegador
- **Dados operacionais**: Ações realizadas no sistema relacionadas com operações portuárias

## 3. Finalidade do Tratamento

Os seus dados pessoais são tratados para:
- Gestão de acesso e autenticação no sistema
- Gestão de operações portuárias e logísticas
- Cumprimento de obrigações legais e regulamentares
- Auditoria e segurança do sistema

## 4. Base Legal

O tratamento dos seus dados baseia-se em:
- Execução de contrato ou diligências pré-contratuais
- Cumprimento de obrigações legais
- Interesses legítimos para segurança e auditoria

## 5. Retenção de Dados

Os dados pessoais são retidos pelo período necessário para cumprir as finalidades descritas, respeitando os prazos legais aplicáveis.

## 6. Seus Direitos

Ao abrigo do RGPD, tem direito a:
- Aceder aos seus dados pessoais
- Retificar dados incorretos
- Solicitar a eliminação dos dados (direito ao esquecimento)
- Opor-se ao tratamento
- Portabilidade dos dados
- Retirar o consentimento

## 7. Contacto

Para exercer os seus direitos ou esclarecer dúvidas sobre privacidade, contacte o Encarregado de Proteção de Dados através do email: dpo@portmanagement.pt

## 8. Alterações à Política

Esta política pode ser atualizada periodicamente. Será notificado de alterações significativas através do sistema.

*Última atualização: Janeiro 2026*',
    N'Versão inicial da política de privacidade conforme RGPD',
    GETUTCDATE(),
    @CreatedBy,
    GETUTCDATE(),
    1,
    'pt'
);

-- Insert English version
SET @PolicyId = NEWID();

INSERT INTO PrivacyPolicies (PolicyId, Version, Content, ChangeSummary, EffectiveDate, CreatedBy, CreatedAt, IsActive, LanguageCode)
VALUES (
    @PolicyId,
    '1.0',
    N'# Privacy Policy

## 1. Introduction

The Port Management System ("System") is committed to protecting your privacy and personal data in compliance with the European Union General Data Protection Regulation (GDPR).

## 2. Data Collected

We collect the following types of personal data:
- **Identification data**: Name, email, user identifier
- **Professional data**: Organization, role, employee number
- **Access data**: Authentication logs, IP address, browser information
- **Operational data**: Actions performed in the system related to port operations

## 3. Purpose of Processing

Your personal data is processed for:
- Access management and system authentication
- Management of port and logistics operations
- Compliance with legal and regulatory obligations
- System audit and security

## 4. Legal Basis

The processing of your data is based on:
- Performance of a contract or pre-contractual measures
- Compliance with legal obligations
- Legitimate interests for security and audit

## 5. Data Retention

Personal data is retained for the period necessary to fulfill the purposes described, respecting applicable legal deadlines.

## 6. Your Rights

Under GDPR, you have the right to:
- Access your personal data
- Rectify incorrect data
- Request data deletion (right to be forgotten)
- Object to processing
- Data portability
- Withdraw consent

## 7. Contact

To exercise your rights or clarify privacy questions, contact the Data Protection Officer at: dpo@portmanagement.pt

## 8. Policy Changes

This policy may be updated periodically. You will be notified of significant changes through the system.

*Last updated: January 2026*',
    N'Initial privacy policy version per GDPR requirements',
    GETUTCDATE(),
    @CreatedBy,
    GETUTCDATE(),
    1,
    'en'
);

SELECT 'Privacy policies seeded successfully!' AS Result;
SELECT * FROM PrivacyPolicies;
