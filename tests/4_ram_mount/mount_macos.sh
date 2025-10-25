# Create RAM disk
diskutil erasevolume HFS+ "GUIDERUN" `hdiutil attach -nomount ram://20480`

cd /Volumes/GUIDERUN

# Copy program and input files into RAM
cp /path/to/guide .
cp /path/to/data.in .
cp /path/to/data.DSC .
cp /path/to/data.csv .

# Run from RAM
./guide < data.in

# Leave and unmount when done
cd ~
diskutil eject /Volumes/GUIDERUN

# List down disks
diskutil list
