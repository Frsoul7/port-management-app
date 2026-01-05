using System;
using System.Threading.Tasks;
using DDDNetCore.Application.DTOs;
using DDDNetCore.Application.Interfaces;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace DDDNetCore.Presentation.Controllers
{
    /// <summary>
    /// Controller for Data Request operations
    /// US 4.5.4: Non-User Data Rights
    /// 
    /// Provides endpoints for:
    /// - Non-users to submit GDPR data requests
    /// - Non-users to check request status
    /// - Admins to manage data requests
    /// </summary>
    [ApiController]
    [Route("api/v1/[controller]")]
    [Produces("application/json")]
    public class DataRequestsController : ControllerBase
    {
        private readonly IDataRequestService _service;

        public DataRequestsController(IDataRequestService service)
        {
            _service = service;
        }

        // ===== Public Endpoints (No Authentication) =====

        /// <summary>
        /// Submit a data request from a non-user (crew member, captain, representative)
        /// </summary>
        /// <remarks>
        /// This endpoint allows individuals who are not registered users but whose data
        /// may have been processed via VVN to exercise their GDPR rights.
        /// 
        /// Requires consent to process the request.
        /// </remarks>
        /// <param name="request">The data request details</param>
        /// <returns>Reference number and estimated response time</returns>
        [HttpPost("non-user")]
        [AllowAnonymous]
        [ProducesResponseType(typeof(DataRequestResponseDto), 201)]
        [ProducesResponseType(400)]
        public async Task<ActionResult<DataRequestResponseDto>> SubmitNonUserRequest([FromBody] NonUserDataRequestDto request)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            if (!request.ConsentGiven)
            {
                return BadRequest(new { message = "Consent must be given to process the data request." });
            }

            try
            {
                // Get client IP for audit logging
                var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();
                
                var response = await _service.SubmitNonUserRequestAsync(request, ipAddress);
                return CreatedAtAction(nameof(CheckRequestStatus), 
                    new { referenceNumber = response.ReferenceNumber, email = request.Email }, 
                    response);
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (Exception ex)
            {
                // Log error (in production, use proper logging)
                Console.Error.WriteLine($"Error submitting data request: {ex}");
                return StatusCode(500, new { message = "An error occurred while processing your request." });
            }
        }

        /// <summary>
        /// Check the status of a data request by reference number and email
        /// </summary>
        /// <remarks>
        /// Allows non-users to track the status of their submitted request.
        /// </remarks>
        [HttpGet("non-user/status")]
        [AllowAnonymous]
        [ProducesResponseType(typeof(DataRequestResponseDto), 200)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<DataRequestResponseDto>> CheckRequestStatus(
            [FromQuery] string referenceNumber, 
            [FromQuery] string email)
        {
            if (string.IsNullOrWhiteSpace(referenceNumber) || string.IsNullOrWhiteSpace(email))
            {
                return BadRequest(new { message = "Reference number and email are required." });
            }

            var response = await _service.CheckRequestStatusAsync(referenceNumber, email);
            
            if (response == null)
            {
                return NotFound(new { message = "No request found with the provided reference number and email." });
            }

            return Ok(response);
        }

        // ===== Admin Endpoints (Require Authentication) =====

        /// <summary>
        /// Get all data requests (Admin only)
        /// </summary>
        [HttpGet]
        [Authorize(Policy = "RequireAdminRole")]
        [ProducesResponseType(typeof(DataRequestDetailsDto[]), 200)]
        [ProducesResponseType(401)]
        [ProducesResponseType(403)]
        public async Task<ActionResult<DataRequestDetailsDto[]>> GetAllRequests()
        {
            var requests = await _service.GetAllRequestsAsync();
            return Ok(requests);
        }

        /// <summary>
        /// Get pending data requests (Admin only)
        /// </summary>
        [HttpGet("pending")]
        [Authorize(Policy = "RequireAdminRole")]
        [ProducesResponseType(typeof(DataRequestDetailsDto[]), 200)]
        [ProducesResponseType(401)]
        [ProducesResponseType(403)]
        public async Task<ActionResult<DataRequestDetailsDto[]>> GetPendingRequests()
        {
            var requests = await _service.GetPendingRequestsAsync();
            return Ok(requests);
        }

        /// <summary>
        /// Get a specific data request by ID (Admin only)
        /// </summary>
        [HttpGet("{id:guid}")]
        [Authorize(Policy = "RequireAdminRole")]
        [ProducesResponseType(typeof(DataRequestDetailsDto), 200)]
        [ProducesResponseType(401)]
        [ProducesResponseType(403)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<DataRequestDetailsDto>> GetRequestById(Guid id)
        {
            var request = await _service.GetRequestByIdAsync(id);
            
            if (request == null)
            {
                return NotFound(new { message = "Data request not found." });
            }

            return Ok(request);
        }

        /// <summary>
        /// Update the status of a data request (Admin only)
        /// </summary>
        /// <remarks>
        /// Allows administrators to update the status of a data request
        /// and add notes about the processing.
        /// </remarks>
        [HttpPut("{id:guid}/status")]
        [Authorize(Policy = "RequireAdminRole")]
        [ProducesResponseType(typeof(DataRequestDetailsDto), 200)]
        [ProducesResponseType(400)]
        [ProducesResponseType(401)]
        [ProducesResponseType(403)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<DataRequestDetailsDto>> UpdateRequestStatus(
            Guid id, 
            [FromBody] UpdateDataRequestStatusDto updateDto)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            // Get admin ID from claims (simplified - in production, use proper claim extraction)
            var adminIdClaim = User.FindFirst("UserId")?.Value ?? User.FindFirst("sub")?.Value;
            if (!Guid.TryParse(adminIdClaim, out var adminId))
            {
                adminId = Guid.Empty; // Fallback for testing
            }

            var result = await _service.UpdateRequestStatusAsync(id, updateDto, adminId);
            
            if (result == null)
            {
                return NotFound(new { message = "Data request not found." });
            }

            return Ok(result);
        }
    }
}
