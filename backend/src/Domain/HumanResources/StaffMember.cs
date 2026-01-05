using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Common;

namespace DDDNetCore.Domain.HumanResources
{
    public class StaffMember
    {
        public long MecanographicNumber { get; private set; } // Unique identifier
        public string ShortName { get; private set; } = default!;
        public string Email { get; private set; } = default!;
        public string Phone { get; private set; } = default!;
        public HumanResourceStatus Status { get; private set; }
        public EntityActiveStatus ActivityStatus { get; private set; }
        public TimeSpan StartHour { get; private set; } // Hora de início
        public TimeSpan EndHour { get; private set; }   // Hora de fim

        public ICollection<StaffMemberQualification> Qualifications { get; private set; } = new List<StaffMemberQualification>();

        private StaffMember() { } // EF Constructor

        public StaffMember(long mecanographicNumber, string shortName, string email, string phone,
                          HumanResourceStatus status, TimeSpan startHour, TimeSpan endHour,
                          EntityActiveStatus activityStatus = EntityActiveStatus.ACTIVE)
        {
            if (mecanographicNumber <= 0) throw new ArgumentException("Mecanographic number must be positive.", nameof(mecanographicNumber));
            if (string.IsNullOrWhiteSpace(shortName)) throw new ArgumentException("Short name is required.", nameof(shortName));
            if (string.IsNullOrWhiteSpace(email)) throw new ArgumentException("Email is required.", nameof(email));

            // Validate email format
            var trimmedEmail = email.Trim();
            if (!System.Text.RegularExpressions.Regex.IsMatch(trimmedEmail, @"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"))
                throw new ArgumentException($"Email must be a valid email address (format: something@something.something). Invalid: {email}", nameof(email));

            if (string.IsNullOrWhiteSpace(phone)) throw new ArgumentException("Phone is required.", nameof(phone));
            if (startHour >= endHour) throw new ArgumentException("Start hour must be earlier than end hour.");

            MecanographicNumber = mecanographicNumber;
            ShortName = shortName.Trim();
            Email = trimmedEmail;
            Phone = phone.Trim();
            Status = status;
            StartHour = startHour;
            EndHour = endHour;
            ActivityStatus = activityStatus;
        }

        public void AddQualification(StaffMemberQualification qualification)
        {
            if (qualification == null) throw new ArgumentNullException(nameof(qualification));
            if (Qualifications.Contains(qualification)) throw new InvalidOperationException("Qualification already assigned to this staff member.");
            Qualifications.Add(qualification);
        }

        public void RemoveQualification(StaffMemberQualification qualification)
        {
            if (qualification == null) throw new ArgumentNullException(nameof(qualification));
            if (!Qualifications.Contains(qualification)) throw new InvalidOperationException("Qualification not found for this staff member.");
            Qualifications.Remove(qualification);
        }

        public void UpdateContactInfo(string email, string phone)
        {
            if (string.IsNullOrWhiteSpace(email)) throw new ArgumentException("Email is required.", nameof(email));

            // Validate email format
            var trimmedEmail = email.Trim();
            if (!System.Text.RegularExpressions.Regex.IsMatch(trimmedEmail, @"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"))
                throw new ArgumentException($"Email must be a valid email address (format: something@something.something). Invalid: {email}", nameof(email));

            if (string.IsNullOrWhiteSpace(phone)) throw new ArgumentException("Phone is required.", nameof(phone));

            Email = trimmedEmail;
            Phone = phone.Trim();
        }

        public void UpdateOperationalStatus(HumanResourceStatus status)
        {
            Status = status;
        }

        public void UpdateActiveStatus(EntityActiveStatus activeStatus)
        {
            ActivityStatus = activeStatus;
        }

        public void UpdateWorkingHours(TimeSpan startHour, TimeSpan endHour)
        {
            if (startHour >= endHour) throw new ArgumentException("Start hour must be earlier than end hour.");
            
            StartHour = startHour;
            EndHour = endHour;
        }

        public bool IsWithinWorkingHours(DateTime dateTime)
        {
            var time = dateTime.TimeOfDay;
            return time >= StartHour && time <= EndHour;
        }
    }
}