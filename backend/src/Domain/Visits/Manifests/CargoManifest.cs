using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Shared;


namespace DDDNetCore.Domain.Visits.Manifests
{
    public abstract class CargoManifest
    {
        public Guid Id { get; protected set; }
        public Guid VvnGuid { get; protected set; }   // EF needs this visible on the base

        protected readonly List<ManifestEntry> _entries = new();
        public IReadOnlyCollection<ManifestEntry> Entries => _entries.AsReadOnly();

        protected CargoManifest() { } // EF

        protected CargoManifest(Guid vvnGuid)
        {
            Id = Guid.NewGuid();
            VvnGuid = vvnGuid;
        }

        public void AddEntry(ManifestEntry entry)
        {
            if (entry == null) throw new ArgumentNullException(nameof(entry));
            if (_entries.Exists(e => e.ContainerUniqueId == entry.ContainerUniqueId))
                throw new InvalidOperationException("Duplicate container in this manifest.");

            // Check for duplicate position (bay/row/tier)
            if (_entries.Exists(e =>
                e.ContainerBayNr == entry.ContainerBayNr &&
                e.ContainerRowNr == entry.ContainerRowNr &&
                e.ContainerTierNr == entry.ContainerTierNr))
                throw new InvalidOperationException($"Position Bay={entry.ContainerBayNr}, Row={entry.ContainerRowNr}, Tier={entry.ContainerTierNr} is already occupied by another container.");

            _entries.Add(entry);
        }

        public void RemoveEntry(Guid entryId)
        {
            var e = _entries.Find(x => x.Id == entryId)
                ?? throw new KeyNotFoundException("Entry not found.");
            _entries.Remove(e);
        }

        // Factory helpers so VVN can create the right derived type
        public static LoadingCargoManifest CreateLoading(Guid vvnGuid) => new LoadingCargoManifest(vvnGuid);
        public static UnloadingCargoManifest CreateUnloading(Guid vvnGuid) => new UnloadingCargoManifest(vvnGuid);
    }

    public sealed class LoadingCargoManifest : CargoManifest
    {
        private LoadingCargoManifest() { } // EF
        internal LoadingCargoManifest(Guid vvnGuid) : base(vvnGuid) { }
    }

    public sealed class UnloadingCargoManifest : CargoManifest
    {
        private UnloadingCargoManifest() { } // EF
        internal UnloadingCargoManifest(Guid vvnGuid) : base(vvnGuid) { }
    }

    public class ManifestEntry
    {
        public Guid Id { get; private set; } = Guid.NewGuid();
        public Guid ManifestId { get; private set; }

        public string ContainerUniqueId { get; private set; } = default!;
        public bool HazardousGoods { get; private set; }
        public int ContainerBayNr { get; private set; }
        public int ContainerRowNr { get; private set; }
        public int ContainerTierNr { get; private set; }
        public string? GoodsDescription { get; private set; }

        private ManifestEntry() { } // EF

        public static ManifestEntry Create(
    string containerUniqueId, bool hazardous, int bay, int row, int tier, string? goods = null)
        {
            if (string.IsNullOrWhiteSpace(containerUniqueId))
                throw new ArgumentException("ContainerUniqueId is required.", nameof(containerUniqueId));

            var code = containerUniqueId.Trim().ToUpperInvariant();
            if (!Iso6346.IsValid(code))
                throw new ArgumentException("ContainerUniqueId is not a valid ISO 6346 code (owner+category+serial+check digit).", nameof(containerUniqueId));

            return new ManifestEntry
            {
                ContainerUniqueId = code,
                HazardousGoods = hazardous,
                ContainerBayNr = bay,
                ContainerRowNr = row,
                ContainerTierNr = tier,
                GoodsDescription = goods
            };
        }
    }
}
