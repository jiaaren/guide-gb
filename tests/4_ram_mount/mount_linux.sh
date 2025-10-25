mkdir /mnt/GUIDERUN
sudo mount -t tmpfs -o size=10M tmpfs /mnt/GUIDERUN

cd /mnt/GUIDERUN
cp /path/to/data.in  .
cp /path/to/data.DSC .
cp /path/to/data.csv .

guide < data.in

cd ~
sudo umount /mnt/GUIDERUN
