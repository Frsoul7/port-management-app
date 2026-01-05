using System.Threading.Tasks;

namespace DDDNetCore.Application.Interfaces
{
    public interface IEmailService
    {
        Task<bool> SendActivationEmailAsync(string toEmail, string toName, string activationLink);
        Task<bool> SendEmailAsync(string toEmail, string subject, string htmlBody);
    }
}
