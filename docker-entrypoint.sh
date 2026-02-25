#!/bin/bash
set -e

DATA_DIR="/srv/shiny-server/data"
SEED_DIR="/seed-data"

# Ensure data directory exists
mkdir -p "$DATA_DIR"

# If volume is empty, seed it
if [ -z "$(ls -A $DATA_DIR)" ]; then
    echo "Seeding initial data..."
    cp -r "$SEED_DIR"/* "$DATA_DIR"/
else
    echo "Data directory not empty, skipping seed."
fi

# Fix ownership
chown -R shiny:shiny "$DATA_DIR"

# Start Shiny server as shiny user
exec su - shiny -c "/usr/bin/shiny-server"
