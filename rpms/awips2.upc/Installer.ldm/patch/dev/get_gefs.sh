#!/bin/bash
# This script is to be run via cron to scrapte the nomads server for the GFS Ensemble data
# ex. */10 * * * * bash -l /awips2/ldm/dev/get_gefs.sh >>/awips2/ldm/dev/logs/gfs_cron.log  2>&1


BASE_URL="https://nomads.ncep.noaa.gov/pub/data/nccf/com/gens/prod"
BASE_OUT="/awips2/data_store/grid/gefs"
LOG="/awips2/ldm/dev/logs/gefs_download.log"

DATE=$(date -u +%Y%m%d)

# Check if EDEX is running
if ! pgrep -f "Dedex.run.mode=ingestGrib" >/dev/null; then
    echo "$(date -u) EDEX not running, skipping download" >> "$LOG"
    exit 0
fi

# Determine current cycle
HOUR=$(date -u +%H)
CURRENT_CYCLE=$((10#$HOUR / 6 * 6))
CURRENT_CYCLE=$(printf "%02d" $CURRENT_CYCLE)

# Determine previous cycle
PREV_CYCLE=$((CURRENT_CYCLE - 6))
if [ $PREV_CYCLE -lt 0 ]; then
    PREV_CYCLE=18
    DATE_PREV=$(date -u -d "yesterday" +%Y%m%d)
else
    DATE_PREV=$DATE
fi
PREV_CYCLE=$(printf "%02d" $PREV_CYCLE)

MAX_JOBS=6

wait_for_slot() {
    while [ $(jobs -rp | wc -l) -ge $MAX_JOBS ]; do
        sleep 1
    done
}

# Function: check if cycle is complete
is_cycle_complete() {
    local dir=$1
    local cycle=$2

    if ls "$dir"/gep01.t${cycle}z.pgrb2a.0p50.f384.grib2 >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# Prevent overlapping runs
exec 9>/tmp/gefs.lock
flock -n 9 || exit 1

echo "==== $(date -u) START ====" >> "$LOG"

# Loop current + previous cycle
for CYCLE in $CURRENT_CYCLE $PREV_CYCLE; do

    if [ "$CYCLE" == "$PREV_CYCLE" ]; then
        USE_DATE=$DATE_PREV
    else
        USE_DATE=$DATE
    fi

    REMOTE_DIR="${BASE_URL}/gefs.${USE_DATE}/${CYCLE}/atmos/pgrb2ap5"
    OUTDIR="${BASE_OUT}/${USE_DATE}/${CYCLE}"

    mkdir -p "$OUTDIR"

    # Skip if already marked complete
    if [ -f "$OUTDIR/.complete" ]; then
        echo "Skipping completed cycle $CYCLE ($USE_DATE)" >> "$LOG"
        continue
    fi

    # Check if cycle complete now
    if is_cycle_complete "$OUTDIR" "$CYCLE"; then
        echo "Marking cycle complete $CYCLE ($USE_DATE)" >> "$LOG"
        touch "$OUTDIR/.complete"
        continue
    fi

    echo "Checking $REMOTE_DIR" >> "$LOG"

    # Single request to NOMADS
    HTML=$(curl -s "$REMOTE_DIR/")

    # Extract valid files (gep01–gep30 only, no .idx)
    FILELIST=$(echo "$HTML" | grep -oE 'gep(0[0-9]|[12][0-9]|30)\.t[0-9]{2}z\.pgrb2a\.0p50\.f[0-9]{3}' | sort -u)

    # Skip if nothing found
    if [ -z "$FILELIST" ]; then
        echo "No files found for $CYCLE ($USE_DATE)" >> "$LOG"
        continue
    fi

    for FILE in $FILELIST; do

        OUTFILE="$OUTDIR/${FILE}.grib2"

        # Skip if already downloaded
        if [ -f "$OUTFILE" ]; then
            continue
        fi

        wait_for_slot

        (
            URL="$REMOTE_DIR/$FILE"

            echo "Downloading $FILE" >> "$LOG"

            curl -s --fail \
                 --connect-timeout 10 \
                 --max-time 300 \
                 --retry 3 \
                 --retry-delay 15 \
                 -o "$OUTFILE" "$URL"

            if [ $? -eq 0 ]; then
                echo "SUCCESS $FILE" >> "$LOG"

                /awips2/python/bin/python /awips2/fxa/bin/src/qpidNotify/qpidNotify.py $OUTFILE
            # Ingest into AWIPS via LDM
            #pqinsert -p "GEFS" -f EXP "$OUTFILE"
            else
                echo "FAILED $FILE" >> "$LOG"
                rm -f "$OUTFILE"
          fi
        ) &
    done
wait
done

echo "==== $(date -u) END ====" >> "$LOG"
