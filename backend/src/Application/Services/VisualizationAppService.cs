using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using DDDNetCore.Application.DTOs.Visualization;

namespace DDDNetCore.Application.Services
{
    /// <summary>
    /// Application service para fornecer dados de layout 3D do porto.
    /// Neste momento devolve um layout estático (placeholders),
    /// apenas para permitir a integração com o frontend.
    /// </summary>
    public class VisualizationAppService
    {
        public Task<PortLayoutDto> GetPortLayoutAsync(CancellationToken ct)
        {
            var layout = new PortLayoutDto
            {
                Docks = new List<DockLayoutDto>
                {
                    new DockLayoutDto
                    {
                        DockId = "D1",
                        Name = "Dock 1",
                        PositionX = 0,
                        PositionY = 0,
                        PositionZ = 0,
                        Length   = 80,
                        Width    = 20,
                        Height   = 5
                    },
                    new DockLayoutDto
                    {
                        DockId = "D2",
                        Name = "Dock 2",
                        PositionX = 120,   // afastada no eixo X
                        PositionY = 0,
                        PositionZ = 0,
                        Length   = 80,
                        Width    = 20,
                        Height   = 5
                    }
                },

                StorageAreas = new List<StorageAreaLayoutDto>
                {
                    new StorageAreaLayoutDto
                    {
                        StorageAreaId = "SA1",
                        Name = "Yard A",
                        Type = "YARD",
                        PositionX = 0,
                        PositionY = 0,
                        PositionZ = 80,
                        Length   = 50,
                        Width    = 40,
                        Height   = 5
                    },
                    new StorageAreaLayoutDto
                    {
                        StorageAreaId = "SA2",
                        Name = "Warehouse B",
                        Type = "WAREHOUSE",
                        PositionX = 120,
                        PositionY = 0,
                        PositionZ = 80,
                        Length   = 50,
                        Width    = 40,
                        Height   = 20
                    }
                }
            };

            return Task.FromResult(layout);
        }

        /// <summary>
        /// Devolve objetos "vivos" para a cena 3D (navios + recursos).
        /// Nesta fase ainda é estático, apenas para integração frontend.
        /// </summary>
        public LiveObjectsDto GetLiveObjects()
        {
            return new LiveObjectsDto
            {
                Vessels =
                {
                    new Vessel3DDto
                    {
                        Id = Guid.NewGuid(),
                        Name = "MSC Example",
                        DockId = "D1",
                        Length = 120,
                        Width = 24,
                        Height = 18
                    }
                },
                Resources =
                {
                    new Resource3DDto
                    {
                        Id = Guid.NewGuid(),
                        Type = "STS_CRANE",
                        AssignedAreaId = "D1"
                    },
                    new Resource3DDto
                    {
                        Id = Guid.NewGuid(),
                        Type = "YARD_GANTRY",
                        AssignedAreaId = "SA1"
                    }
                }
            };
        }
    }
}
