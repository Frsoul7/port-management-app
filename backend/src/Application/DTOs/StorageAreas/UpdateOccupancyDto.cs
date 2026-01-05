using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.StorageAreas;

public record UpdateOccupancyDto(
    [Range(0, int.MaxValue)] int NewOccupancyTEU
);
