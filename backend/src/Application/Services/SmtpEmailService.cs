using DDDNetCore.Application.Interfaces;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using System;
using System.Net;
using System.Net.Mail;
using System.Threading.Tasks;

namespace DDDNetCore.Application.Services
{
    public class SmtpEmailService : IEmailService
    {
        private readonly IConfiguration _configuration;
        private readonly ILogger<SmtpEmailService> _logger;

        public SmtpEmailService(IConfiguration configuration, ILogger<SmtpEmailService> logger)
        {
            _configuration = configuration;
            _logger = logger;
        }

        public async Task<bool> SendActivationEmailAsync(string toEmail, string toName, string activationLink)
        {
            var subject = "Activate Your Port Management System Account";
            var htmlBody = $@"
<!DOCTYPE html>
<html>
<head>
    <style>
        body {{ font-family: Arial, sans-serif; line-height: 1.6; color: #333; }}
        .container {{ max-width: 600px; margin: 0 auto; padding: 20px; }}
        .header {{ background-color: #667eea; color: white; padding: 20px; text-align: center; border-radius: 5px 5px 0 0; }}
        .content {{ background-color: #f9f9f9; padding: 30px; border-radius: 0 0 5px 5px; }}
        .button {{ display: inline-block; padding: 12px 30px; background-color: #667eea; color: white; text-decoration: none; border-radius: 5px; margin: 20px 0; }}
        .footer {{ text-align: center; margin-top: 20px; font-size: 12px; color: #666; }}
        .warning {{ background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 12px; margin: 20px 0; }}
    </style>
</head>
<body>
    <div class='container'>
        <div class='header'>
            <h1>Welcome to Port Management System</h1>
        </div>
        <div class='content'>
            <h2>Hello {toName},</h2>
            <p>Thank you for registering with Port Management System. To complete your registration and access the system, please activate your account by clicking the button below:</p>
            
            <div style='text-align: center;'>
                <a href='{activationLink}' class='button'>Activate Account</a>
            </div>
            
            <p>Or copy and paste this link into your browser:</p>
            <p style='background-color: #e9ecef; padding: 10px; word-break: break-all; font-size: 12px;'>{activationLink}</p>
            
            <div class='warning'>
                <strong>⚠️ Important:</strong> This activation link will expire in 24 hours. If you don't activate your account within this time, you'll need to register again.
            </div>
            
            <p>After activation, you'll be redirected to sign in with your organization's identity provider.</p>
            
            <p>If you didn't create an account with us, please ignore this email.</p>
            
            <p>Best regards,<br>Port Management System Team</p>
        </div>
        <div class='footer'>
            <p>&copy; {DateTime.UtcNow.Year} Port Management System. All rights reserved.</p>
        </div>
    </div>
</body>
</html>";

            return await SendEmailAsync(toEmail, subject, htmlBody);
        }

        public async Task<bool> SendEmailAsync(string toEmail, string subject, string htmlBody)
        {
            try
            {
                // Get SMTP configuration from environment variables (priority) or appsettings.json
                var smtpHost = Environment.GetEnvironmentVariable("SMTP_HOST")
                    ?? _configuration["Smtp:Host"];
                var smtpPortStr = Environment.GetEnvironmentVariable("SMTP_PORT")
                    ?? _configuration["Smtp:Port"];
                var smtpUsername = Environment.GetEnvironmentVariable("SMTP_USERNAME")
                    ?? _configuration["Smtp:Username"];
                var smtpPassword = Environment.GetEnvironmentVariable("SMTP_PASSWORD")
                    ?? _configuration["Smtp:Password"];
                var smtpFromEmail = Environment.GetEnvironmentVariable("SMTP_FROM_EMAIL")
                    ?? _configuration["Smtp:FromEmail"];
                var smtpFromName = Environment.GetEnvironmentVariable("SMTP_FROM_NAME")
                    ?? _configuration["Smtp:FromName"]
                    ?? "Port Management System";
                var enableSslStr = Environment.GetEnvironmentVariable("SMTP_ENABLE_SSL")
                    ?? _configuration["Smtp:EnableSsl"]
                    ?? "true";

                // Validate configuration
                if (string.IsNullOrWhiteSpace(smtpHost))
                {
                    _logger.LogError("SMTP Host not configured. Set SMTP_HOST environment variable or Smtp:Host in appsettings.json");
                    return false;
                }

                if (!int.TryParse(smtpPortStr, out var smtpPort))
                {
                    _logger.LogError("Invalid SMTP Port. Set SMTP_PORT environment variable or Smtp:Port in appsettings.json");
                    return false;
                }

                if (string.IsNullOrWhiteSpace(smtpFromEmail))
                {
                    _logger.LogError("SMTP From Email not configured. Set SMTP_FROM_EMAIL environment variable or Smtp:FromEmail in appsettings.json");
                    return false;
                }

                bool.TryParse(enableSslStr, out var enableSsl);

                _logger.LogInformation("Sending email to {ToEmail} via {SmtpHost}:{SmtpPort}", toEmail, smtpHost, smtpPort);

                using var mailMessage = new MailMessage
                {
                    From = new MailAddress(smtpFromEmail, smtpFromName),
                    Subject = subject,
                    Body = htmlBody,
                    IsBodyHtml = true
                };

                mailMessage.To.Add(toEmail);

                using var smtpClient = new SmtpClient(smtpHost, smtpPort)
                {
                    EnableSsl = enableSsl,
                    DeliveryMethod = SmtpDeliveryMethod.Network,
                    UseDefaultCredentials = false
                };

                // Only set credentials if username and password are provided
                if (!string.IsNullOrWhiteSpace(smtpUsername) && !string.IsNullOrWhiteSpace(smtpPassword))
                {
                    smtpClient.Credentials = new NetworkCredential(smtpUsername, smtpPassword);
                }

                await smtpClient.SendMailAsync(mailMessage);

                _logger.LogInformation("Email successfully sent to {ToEmail}", toEmail);
                return true;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Failed to send email to {ToEmail}", toEmail);
                return false;
            }
        }
    }
}
