# TF SAFE CMMS - Deployment Guide

## Building the Deployment Package

On your development machine (where SBCL and Quicklisp are installed):

```bash
cd /path/to/tfs-cmms
make package
```

This creates a `dist/` folder containing everything needed to run the application.

To create a distributable tarball:

```bash
make dist
```

This creates `tfs-cmms-YYYYMMDD.tar.gz`.

## Deployment Package Contents

```text
dist/
├── tfs-cmms          # Standalone executable (~80-150MB)
├── start.sh          # Startup script
├── static/           # CSS, JavaScript, images
│   ├── css/
│   ├── js/
│   └── img/
└── data/             # Database directory (created automatically)
```

## Server Requirements

- **OS**: Linux (64-bit) - same architecture as build machine
- **RAM**: 512MB minimum, 1GB recommended
- **Disk**: 200MB for application + database growth
- **Ports**: 8080 (configurable)

No additional software required - the executable is self-contained.

## Installation Steps

1. **Copy files to server**:

   ```bash
   scp tfs-cmms-YYYYMMDD.tar.gz user@server:/opt/
   ```

2. **Extract on server**:

   ```bash
   cd /opt
   tar -xzvf tfs-cmms-YYYYMMDD.tar.gz
   mv dist tfs-cmms
   ```

3. **Start the application**:

   ```bash
   cd /opt/tfs-cmms
   ./start.sh
   ```

4. **Access the application**:

   Open browser to `http://server-ip:8080`

## Running as a Service (systemd)

Create `/etc/systemd/system/tfs-cmms.service`:

```ini
[Unit]
Description=TF SAFE CMMS
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/opt/tfs-cmms
ExecStart=/opt/tfs-cmms/tfs-cmms
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

Enable and start:

```bash
sudo systemctl daemon-reload
sudo systemctl enable tfs-cmms
sudo systemctl start tfs-cmms
```

Check status:

```bash
sudo systemctl status tfs-cmms
```

## Changing the Port

Edit the executable startup or use a reverse proxy (nginx/Apache) to map port 80/443 to 8080.

### Nginx Reverse Proxy Example

```nginx
server {
    listen 80;
    server_name cmms.example.com;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

## Database Backup

The SQLite database is stored at `data/tfs-cmms.db`. To backup:

```bash
cp /opt/tfs-cmms/data/tfs-cmms.db /backup/tfs-cmms-$(date +%Y%m%d).db
```

## Troubleshooting

### Application won't start

- Check permissions: `chmod +x tfs-cmms start.sh`
- Check if port 8080 is in use: `netstat -tlnp | grep 8080`

### Database errors

- Ensure `data/` directory exists and is writable
- Check disk space

### Static files not loading

- Verify `static/` folder is in same directory as executable
- Check file permissions

## Cross-Platform Notes

The executable is built for the same OS/architecture as the build machine. If deploying to a different Linux distribution, it should work as long as:

- Same CPU architecture (x86_64)
- glibc version is compatible (build on older system for wider compatibility)

For other platforms (Windows, macOS), rebuild on that platform.
