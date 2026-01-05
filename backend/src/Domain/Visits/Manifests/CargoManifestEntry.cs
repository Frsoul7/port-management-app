public class CargoManifestEntry
{
    public Guid Id { get; private set; } = Guid.NewGuid();

    public string ContainerUniqueId { get; private set; } = default!;
    public bool HazardousGoods { get; private set; }
    public int ContainerBayNr { get; private set; }
    public int ContainerRowNr { get; private set; }
    public int ContainerTierNr { get; private set; }
    public string? GoodsDescription { get; private set; }

    protected CargoManifestEntry() { }

    public static CargoManifestEntry Create(
        string containerUniqueId,
        bool hazardousGoods,
        int bayNr,
        int rowNr,
        int tierNr,
        string? goodsDescription = null)
    {
        return new CargoManifestEntry
        {
            ContainerUniqueId = containerUniqueId,
            HazardousGoods = hazardousGoods,
            ContainerBayNr = bayNr,
            ContainerRowNr = rowNr,
            ContainerTierNr = tierNr,
            GoodsDescription = goodsDescription
        };
    }
}
