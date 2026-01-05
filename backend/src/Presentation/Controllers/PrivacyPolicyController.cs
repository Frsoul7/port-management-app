using DDDNetCore.Application.DTOs.PrivacyPolicy;
using DDDNetCore.Application.Interfaces;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Security.Claims;
using System.Threading.Tasks;

namespace DDDNetCore.Presentation.Controllers
{
    /// <summary>
    /// Controller for Privacy Policy management and user acknowledgment.
    /// US 4.5.1: As an Administrator, I want to publish and manage the system's Privacy Policy.
    /// US 4.5.2: As a System User, I want clear and accessible information about personal data processing.
    /// </summary>
    [ApiController]
    [Route("api/[controller]")]
    public class PrivacyPolicyController : ControllerBase
    {
        private readonly IPrivacyPolicyService _privacyPolicyService;

        public PrivacyPolicyController(IPrivacyPolicyService privacyPolicyService)
        {
            _privacyPolicyService = privacyPolicyService;
        }

        // ===== Public Endpoints (US 4.5.2) =====

        /// <summary>
        /// Get the current active privacy policy.
        /// Public endpoint - accessible without authentication.
        /// </summary>
        /// <param name="lang">Language code (default: "pt")</param>
        /// <returns>Current privacy policy or 404 if none exists</returns>
        [HttpGet("current")]
        [AllowAnonymous]
        [ProducesResponseType(typeof(PrivacyPolicyDto), 200)]
        [ProducesResponseType(404)]
        public async Task<IActionResult> GetCurrentPolicy([FromQuery] string lang = "pt")
        {
            var policy = await _privacyPolicyService.GetCurrentPolicyAsync(lang);
            
            if (policy == null)
            {
                return NotFound(new { message = "No active privacy policy found" });
            }

            return Ok(policy);
        }

        // ===== Authenticated User Endpoints =====

        /// <summary>
        /// Check if the current user needs to acknowledge a new privacy policy.
        /// Called on login to determine if the acknowledgment modal should be shown.
        /// </summary>
        /// <param name="lang">Language code (default: "pt")</param>
        /// <returns>Acknowledgment status</returns>
        [HttpGet("acknowledgment-required")]
        [Authorize]
        [ProducesResponseType(typeof(AcknowledgmentStatusDto), 200)]
        [ProducesResponseType(401)]
        public async Task<IActionResult> CheckAcknowledgmentRequired([FromQuery] string lang = "pt")
        {
            var userId = GetCurrentUserId();
            if (string.IsNullOrEmpty(userId))
            {
                return Unauthorized(new { message = "User ID not found in token" });
            }

            try
            {
                var status = await _privacyPolicyService.CheckAcknowledgmentRequiredAsync(userId, lang);
                return Ok(status);
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// Record the current user's acknowledgment of a privacy policy.
        /// Called when user clicks "I Acknowledge" in the modal.
        /// </summary>
        /// <param name="request">Acknowledgment request containing the policy ID</param>
        /// <returns>Acknowledgment confirmation</returns>
        [HttpPost("acknowledge")]
        [Authorize]
        [ProducesResponseType(typeof(AcknowledgmentResponseDto), 200)]
        [ProducesResponseType(400)]
        [ProducesResponseType(401)]
        [ProducesResponseType(404)]
        public async Task<IActionResult> AcknowledgePolicy([FromBody] AcknowledgePolicyRequest request)
        {
            var userId = GetCurrentUserId();
            if (string.IsNullOrEmpty(userId))
            {
                return Unauthorized(new { message = "User ID not found in token" });
            }

            // Get client IP and user agent for audit
            var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();
            var userAgent = Request.Headers["User-Agent"].ToString();

            try
            {
                var response = await _privacyPolicyService.AcknowledgePolicyAsync(
                    userId, request, ipAddress, userAgent);
                return Ok(response);
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
        }

        // ===== Admin Endpoints (US 4.5.1) =====

        /// <summary>
        /// Get all privacy policy versions (history).
        /// Admin only.
        /// </summary>
        /// <param name="lang">Optional language code filter</param>
        /// <returns>List of policy summaries</returns>
        [HttpGet("history")]
        [Authorize]
        [ProducesResponseType(typeof(PrivacyPolicySummaryDto[]), 200)]
        [ProducesResponseType(401)]
        [ProducesResponseType(403)]
        public async Task<IActionResult> GetPolicyHistory([FromQuery] string? lang = null)
        {
            if (!User.IsInRole("ADMINISTRATOR"))
            {
                return Forbid();
            }

            var history = await _privacyPolicyService.GetPolicyHistoryAsync(lang);
            return Ok(history);
        }

        /// <summary>
        /// Get a specific policy by ID (including content).
        /// Admin only.
        /// </summary>
        /// <param name="id">Policy ID</param>
        /// <returns>Full policy details</returns>
        [HttpGet("{id}")]
        [Authorize]
        [ProducesResponseType(typeof(PrivacyPolicyDto), 200)]
        [ProducesResponseType(401)]
        [ProducesResponseType(403)]
        [ProducesResponseType(404)]
        public async Task<IActionResult> GetPolicyById(string id)
        {
            if (!User.IsInRole("ADMINISTRATOR"))
            {
                return Forbid();
            }

            var policy = await _privacyPolicyService.GetPolicyByIdAsync(id);
            
            if (policy == null)
            {
                return NotFound(new { message = "Policy not found" });
            }

            return Ok(policy);
        }

        /// <summary>
        /// Publish a new privacy policy version.
        /// Admin only. This triggers the acknowledgment requirement for all users.
        /// </summary>
        /// <param name="request">New policy details</param>
        /// <returns>Created policy</returns>
        [HttpPost]
        [Authorize]
        [ProducesResponseType(typeof(PrivacyPolicyDto), 201)]
        [ProducesResponseType(400)]
        [ProducesResponseType(401)]
        [ProducesResponseType(403)]
        public async Task<IActionResult> PublishPolicy([FromBody] PublishPrivacyPolicyRequest request)
        {
            if (!User.IsInRole("ADMINISTRATOR"))
            {
                return Forbid();
            }

            var adminUserId = GetCurrentUserId();
            if (string.IsNullOrEmpty(adminUserId))
            {
                return Unauthorized(new { message = "Admin user ID not found in token" });
            }

            try
            {
                var policy = await _privacyPolicyService.PublishNewPolicyAsync(request, adminUserId);
                return CreatedAtAction(nameof(GetPolicyById), new { id = policy.PolicyId }, policy);
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// Get acknowledgments for a specific policy.
        /// Admin only. For reporting purposes.
        /// </summary>
        /// <param name="id">Policy ID</param>
        /// <returns>List of acknowledgments</returns>
        [HttpGet("{id}/acknowledgments")]
        [Authorize]
        [ProducesResponseType(typeof(PolicyAcknowledgmentDto[]), 200)]
        [ProducesResponseType(401)]
        [ProducesResponseType(403)]
        public async Task<IActionResult> GetPolicyAcknowledgments(string id)
        {
            if (!User.IsInRole("ADMINISTRATOR"))
            {
                return Forbid();
            }

            try
            {
                var acknowledgments = await _privacyPolicyService.GetPolicyAcknowledgmentsAsync(id);
                return Ok(acknowledgments);
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        // ===== Helper Methods =====

        private string? GetCurrentUserId()
        {
            // Try standard claim first
            var userId = User.FindFirst(ClaimTypes.NameIdentifier)?.Value;
            
            // Fallback to custom claim used in JWT
            if (string.IsNullOrEmpty(userId))
            {
                userId = User.FindFirst("userId")?.Value;
            }

            return userId;
        }
    }
}
