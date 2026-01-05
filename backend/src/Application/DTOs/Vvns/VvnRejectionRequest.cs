namespace DDDNetCore.API.Contracts
{
    /// <summary>
    /// Data required for VVN rejection (renamed from RejectVvnRequest - Phase 5: DTO naming fix)
    /// </summary>
    public class VvnRejectionRequest
    {
        public string Reason { get; set; } = null!;
        public string? Notes { get; set; }
    }
}
