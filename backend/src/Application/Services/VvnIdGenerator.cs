using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;
using DDDNetCore.Domain.IRepository;

namespace DDDNetCore.Application.Services
{
    /// <summary>
    /// Generates unique business identifiers for Vessel Visit Notifications
    /// following the pattern: {YEAR}-{PORT_CODE}-{SEQUENTIAL_NUMBER}
    /// Example: 2025-PTLEI-000001
    /// </summary>
    public class VvnIdGenerator
    {
        private readonly IVesselVisitNotificationRepository _vvnRepository;
        private readonly string _portCode;

        public VvnIdGenerator(IVesselVisitNotificationRepository vvnRepository, IConfiguration config)
        {
            _vvnRepository = vvnRepository;
            _portCode = config["PortSettings:PortCode"] 
                ?? throw new InvalidOperationException("PortSettings:PortCode must be configured in appsettings.json");
        }

        /// <summary>
        /// Generates the next sequential VVN business ID for the given year.
        /// Sequential number resets each year.
        /// </summary>
        public async Task<string> GenerateNextIdAsync(int year)
        {
            // Get all VVN IDs for this year and port
            var prefix = $"{year}-{_portCode}-";
            
            var existingIds = await _vvnRepository.GetBusinessIdsByPrefixAsync(prefix);

            // Find the maximum sequential number
            int maxSeq = 0;
            foreach (var id in existingIds)
            {
                // Extract sequential part: "2025-PTLEI-000123" -> "000123"
                var parts = id.Split('-');
                if (parts.Length == 3 && int.TryParse(parts[2], out int seq))
                {
                    if (seq > maxSeq) maxSeq = seq;
                }
            }

            // Next sequential number
            int nextSeq = maxSeq + 1;

            // Format: 2025-PTLEI-000001 (6 digits, zero-padded)
            return $"{year}-{_portCode}-{nextSeq:D6}";
        }
    }
}
