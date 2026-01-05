using System;
using System.Threading.Tasks;
using DDDNetCore.Application.DTOs;
using DDDNetCore.Application.Interfaces;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace DDDNetCore.Presentation.Controllers
{
    /// <summary>
    /// Controller for User Data Rights operations (GDPR Subject Access Requests)
    /// US 4.5.3: User Data Rights (SAR)
    /// 
    /// Provides endpoints for:
    /// - Data export (JSON/PDF) - GDPR Article 15
    /// - Rectification requests - GDPR Article 16
    /// - Deletion requests - GDPR Article 17
    /// - Request history
    /// </summary>
    [ApiController]
    [Route("api/v1/[controller]")]
    [Produces("application/json")]
    public class DataRightsController : ControllerBase
    {
        private readonly IDataRightsService _dataRightsService;

        public DataRightsController(IDataRightsService dataRightsService)
        {
            _dataRightsService = dataRightsService;
        }

        /// <summary>
        /// Export user's data as JSON (GDPR Article 15 - Right of Access)
        /// </summary>
        /// <returns>Complete user data export in JSON format</returns>
        /// <response code="200">Data export successful</response>
        /// <response code="401">User not authenticated</response>
        /// <response code="404">User not found</response>
        [HttpGet("export")]
        [Authorize]
        [ProducesResponseType(typeof(UserDataExportDto), 200)]
        [ProducesResponseType(401)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<UserDataExportDto>> ExportDataJson()
        {
            var userId = GetCurrentUserId();
            if (userId == null)
            {
                return Unauthorized();
            }

            try
            {
                var data = await _dataRightsService.ExportUserDataAsync(userId.Value);
                return Ok(data);
            }
            catch (InvalidOperationException ex) when (ex.Message.Contains("not found"))
            {
                return NotFound(new { message = ex.Message });
            }
        }

        /// <summary>
        /// Export user's data as PDF (GDPR Article 15 - Right of Access)
        /// </summary>
        /// <returns>PDF file containing user data</returns>
        /// <response code="200">PDF export successful</response>
        /// <response code="401">User not authenticated</response>
        /// <response code="404">User not found</response>
        [HttpGet("export/pdf")]
        [Authorize]
        [ProducesResponseType(typeof(FileContentResult), 200)]
        [ProducesResponseType(401)]
        [ProducesResponseType(404)]
        public async Task<IActionResult> ExportDataPdf()
        {
            var userId = GetCurrentUserId();
            if (userId == null)
            {
                return Unauthorized();
            }

            try
            {
                var pdfBytes = await _dataRightsService.ExportUserDataAsPdfAsync(userId.Value);
                var fileName = $"gdpr_data_export_{DateTime.UtcNow:yyyyMMdd_HHmmss}.pdf";
                return File(pdfBytes, "application/pdf", fileName);
            }
            catch (InvalidOperationException ex) when (ex.Message.Contains("not found"))
            {
                return NotFound(new { message = ex.Message });
            }
        }

        /// <summary>
        /// Submit a data rectification request (GDPR Article 16 - Right to Rectification)
        /// </summary>
        /// <param name="request">Rectification request details</param>
        /// <returns>Created request confirmation</returns>
        /// <response code="201">Rectification request submitted</response>
        /// <response code="400">Invalid request</response>
        /// <response code="401">User not authenticated</response>
        [HttpPost("rectification")]
        [Authorize]
        [ProducesResponseType(typeof(UserDataRequestResponseDto), 201)]
        [ProducesResponseType(400)]
        [ProducesResponseType(401)]
        public async Task<ActionResult<UserDataRequestResponseDto>> RequestRectification([FromBody] RectificationRequestDto request)
        {
            var userId = GetCurrentUserId();
            if (userId == null)
            {
                return Unauthorized();
            }

            if (string.IsNullOrWhiteSpace(request.DataCategory) ||
                string.IsNullOrWhiteSpace(request.CurrentValue) ||
                string.IsNullOrWhiteSpace(request.RequestedCorrection) ||
                string.IsNullOrWhiteSpace(request.Reason))
            {
                return BadRequest(new { message = "All fields are required for a rectification request." });
            }

            try
            {
                var response = await _dataRightsService.RequestRectificationAsync(userId.Value, request);
                return CreatedAtAction(nameof(GetMyRequests), response);
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// Submit an account deletion request (GDPR Article 17 - Right to Erasure)
        /// </summary>
        /// <param name="request">Deletion request details with confirmation</param>
        /// <returns>Created request confirmation</returns>
        /// <response code="201">Deletion request submitted</response>
        /// <response code="400">Invalid request or pending deletion exists</response>
        /// <response code="401">User not authenticated</response>
        [HttpPost("deletion")]
        [Authorize]
        [ProducesResponseType(typeof(UserDataRequestResponseDto), 201)]
        [ProducesResponseType(400)]
        [ProducesResponseType(401)]
        public async Task<ActionResult<UserDataRequestResponseDto>> RequestDeletion([FromBody] DeletionRequestDto request)
        {
            var userId = GetCurrentUserId();
            if (userId == null)
            {
                return Unauthorized();
            }

            if (!request.ConfirmUnderstanding)
            {
                return BadRequest(new { message = "You must confirm that you understand the consequences of account deletion." });
            }

            try
            {
                var response = await _dataRightsService.RequestDeletionAsync(userId.Value, request);
                return CreatedAtAction(nameof(GetMyRequests), response);
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// Get user's data request history
        /// </summary>
        /// <returns>List of user's data requests</returns>
        /// <response code="200">Request history retrieved</response>
        /// <response code="401">User not authenticated</response>
        [HttpGet("requests")]
        [Authorize]
        [ProducesResponseType(typeof(MyDataRequestDto[]), 200)]
        [ProducesResponseType(401)]
        public async Task<ActionResult<MyDataRequestDto[]>> GetMyRequests()
        {
            var userId = GetCurrentUserId();
            if (userId == null)
            {
                return Unauthorized();
            }

            var requests = await _dataRightsService.GetMyRequestsAsync(userId.Value);
            return Ok(requests);
        }

        /// <summary>
        /// Process a deletion request (Admin only)
        /// </summary>
        /// <param name="requestId">The data request ID</param>
        /// <param name="request">Processing decision</param>
        /// <returns>Updated request details</returns>
        /// <response code="200">Request processed successfully</response>
        /// <response code="400">Invalid request</response>
        /// <response code="401">User not authenticated</response>
        /// <response code="403">User is not an admin</response>
        /// <response code="404">Request not found</response>
        [HttpPost("admin/deletion/{requestId}/process")]
        [Authorize(Policy = "RequireAdminRole")]
        [ProducesResponseType(typeof(DataRequestDetailsDto), 200)]
        [ProducesResponseType(400)]
        [ProducesResponseType(401)]
        [ProducesResponseType(403)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<DataRequestDetailsDto>> ProcessDeletionRequest(
            Guid requestId,
            [FromBody] ProcessDeletionRequestDto request)
        {
            var adminId = GetCurrentUserId();
            if (adminId == null)
            {
                return Unauthorized();
            }

            try
            {
                var result = await _dataRightsService.ProcessDeletionRequestAsync(
                    requestId, 
                    adminId.Value, 
                    request.Approved, 
                    request.Notes);

                if (result == null)
                {
                    return NotFound(new { message = "Data request not found." });
                }

                return Ok(result);
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        private Guid? GetCurrentUserId()
        {
            // Try to get from JWT claims
            var userIdClaim = User.FindFirst("sub") ?? User.FindFirst("userId") ?? User.FindFirst(System.Security.Claims.ClaimTypes.NameIdentifier);
            if (userIdClaim != null && Guid.TryParse(userIdClaim.Value, out var userId))
            {
                return userId;
            }

            // Fallback to X-User-Id header (for development)
            if (Request.Headers.TryGetValue("X-User-Id", out var headerUserId) &&
                Guid.TryParse(headerUserId.FirstOrDefault(), out var headerId))
            {
                return headerId;
            }

            return null;
        }
    }

    /// <summary>
    /// Request DTO for processing deletion requests (admin)
    /// </summary>
    public class ProcessDeletionRequestDto
    {
        /// <summary>Whether to approve the deletion request</summary>
        public bool Approved { get; set; }
        
        /// <summary>Admin notes explaining the decision</summary>
        public string? Notes { get; set; }
    }
}
