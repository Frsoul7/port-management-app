using System;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace DDDNetCore.Domain.Shared
{
    /// <summary>
    /// Formats DateTime and DateTime? with a given format when writing JSON.
    /// DateTime is treated as-is (no timezone conversion).
    /// </summary>
    public sealed class JsonDateTimeConverter : JsonConverter<DateTime>
    {
        private readonly string _format;
        public JsonDateTimeConverter(string format = "yyyy-MM-dd HH:mm:ss") => _format = format;

        public override DateTime Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
            => DateTime.Parse(reader.GetString()!);

        public override void Write(Utf8JsonWriter writer, DateTime value, JsonSerializerOptions options)
            => writer.WriteStringValue(value.ToString(_format));
    }

    public sealed class JsonNullableDateTimeConverter : JsonConverter<DateTime?>
    {
        private readonly string _format;
        public JsonNullableDateTimeConverter(string format = "yyyy-MM-dd HH:mm:ss") => _format = format;

        public override DateTime? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
        {
            var s = reader.GetString();
            return string.IsNullOrWhiteSpace(s) ? (DateTime?)null : DateTime.Parse(s);
        }

        public override void Write(Utf8JsonWriter writer, DateTime? value, JsonSerializerOptions options)
        {
            if (value.HasValue)
                writer.WriteStringValue(value.Value.ToString(_format));
            else
                writer.WriteNullValue();
        }
    }
}
