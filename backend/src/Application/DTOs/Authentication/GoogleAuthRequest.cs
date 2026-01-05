using System.Text.Json.Serialization;

namespace DDDNetCore.Application.DTOs.Authentication
{
    public record GoogleAuthRequest(
        string Code, 
        [property: JsonPropertyName("redirect_uri")] string RedirectUri
    );
}
