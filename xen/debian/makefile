RELEASE = squeeze
NAME = flukso-$(RELEASE)
IMAGE = $(PWD)/$(NAME).img
#MNTPOINT = $(PWD)/mnt
MNTPOINT = /mnt

FS_TYPE = ext4
# A Debian Squeeze base system requires about 200MB.
FS_SIZE = 256 #MB

MS_CONF = flukso.conf
MS_PREFIX = $(PWD)/multistrap-2.1.14/

image: filesystem
	sudo $(MS_PREFIX)multistrap -f $(MS_CONF)

filesystem:
	dd if=/dev/zero of=$(IMAGE) bs=1M count=$(FS_SIZE)
	mke2fs -F -t $(FS_TYPE) -b 4096 -L $(NAME) $(IMAGE)
#	mkdir -p $(MNTPOINT)
	sudo mount -t $(FS_TYPE) -o loop $(IMAGE) $(MNTPOINT) 

umount:
	sudo umount $(MNTPOINT)
#	rmdir $(MNTPOINT)

clean: umount
	rm $(IMAGE)
