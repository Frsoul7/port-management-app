# MongoDB Installation Guide

## Quick Install (Windows)

### Option 1: MongoDB Community Server (Recommended)

1. **Download MongoDB:**
   - Go to: https://www.mongodb.com/try/download/community
   - Select: Windows x64
   - Click: Download

2. **Install:**
   - Run the `.msi` installer
   - Choose "Complete" installation
   - **IMPORTANT:** Check "Install MongoDB as a Service"
   - Keep default data directory: `C:\Program Files\MongoDB\Server\8.0\data`

3. **Verify Installation:**
   ```powershell
   # Check if MongoDB service is running
   Get-Service -Name MongoDB
   
   # Should show: Running
   ```

4. **Test Connection:**
   ```powershell
   # Connect to MongoDB
   mongosh
   
   # You should see MongoDB shell
   # Type 'exit' to quit
   ```

---

### Option 2: Chocolatey (if you have it)

```powershell
# Install MongoDB via Chocolatey
choco install mongodb

# Start MongoDB service
Start-Service MongoDB
```

---

### Option 3: Manual ZIP Installation

1. **Download ZIP:**
   - Go to: https://www.mongodb.com/try/download/community
   - Select: Windows x64 (zip)

2. **Extract:**
   - Extract to: `C:\mongodb`

3. **Create Data Directory:**
   ```powershell
   New-Item -ItemType Directory -Path C:\data\db -Force
   ```

4. **Add to PATH:**
   ```powershell
   # Add MongoDB to system PATH
   $env:Path += ";C:\mongodb\bin"
   
   # Make it permanent (run as Administrator)
   [Environment]::SetEnvironmentVariable("Path", $env:Path + ";C:\mongodb\bin", "Machine")
   ```

5. **Start MongoDB:**
   ```powershell
   mongod --dbpath C:\data\db
   ```

---

## After Installation

### 1. Verify MongoDB is Running

```powershell
# Check MongoDB service
Get-Service -Name MongoDB

# Or check process
Get-Process mongod
```

### 2. Test OEM Backend Connection

```powershell
cd backend-oem
npm run dev

# Should see:
# ✓ Connected to MongoDB successfully
# ✓ OEM Backend server listening on port 3000
```

### 3. Create Initial Database

```powershell
# Connect to MongoDB
mongosh

# Create OEM database
use oem

# Verify
show dbs
exit
```

---

## Troubleshooting

### MongoDB Service Won't Start

```powershell
# Check if port 27017 is in use
Get-NetTCPConnection -LocalPort 27017

# If occupied, kill the process
Stop-Process -Id <ProcessId> -Force

# Start MongoDB service
Start-Service MongoDB
```

### Connection Refused Error

Check `backend-oem/.env` has correct MongoDB URL:
```
DATABASE_URL=mongodb://localhost:27017/oem
```

### Data Directory Permissions

```powershell
# Create directory with proper permissions
New-Item -ItemType Directory -Path C:\data\db -Force
icacls "C:\data\db" /grant Everyone:F
```

---

## Quick Test Script

After installing MongoDB, test everything:

```powershell
# Run the startup script
.\START_ALL.ps1

# In another terminal, test endpoints
Invoke-WebRequest -Uri "https://localhost:5001/health" -SkipCertificateCheck
Invoke-WebRequest -Uri "http://localhost:3000/health"
```

---

## Alternative: MongoDB Atlas (Cloud - No Installation)

If you don't want to install locally, use MongoDB Atlas (free tier):

1. Go to: https://www.mongodb.com/cloud/atlas/register
2. Create free cluster
3. Get connection string
4. Update `backend-oem/.env`:
   ```
   DATABASE_URL=mongodb+srv://<username>:<password>@cluster0.xxxxx.mongodb.net/oem?retryWrites=true&w=majority
   ```

---

## After Instalation

Once MongoDB is running:
1. Run `.\START_ALL.ps1`
2. Follow `INTEGRATION_TESTING_GUIDE.md`
3. Test all 20 API endpoints
4. Verify data persistence
