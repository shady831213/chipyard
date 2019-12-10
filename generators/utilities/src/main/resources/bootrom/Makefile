bootrom_img = bootrom.img
bootrom_dump = bootrom.dump

GCC=riscv64-unknown-elf-gcc
CFLAGS=-mabi=lp64 -march=rv64ima
OBJCOPY=riscv64-unknown-elf-objcopy
OBJDUMP=riscv64-unknown-elf-objdump

all: $(bootrom_img) $(bootrom_dump)

%.img: %.elf
	$(OBJCOPY) -O binary --change-addresses=-0x10000 $< $@

%.elf: %.S linker.ld
	$(GCC) $(CFLAGS) -Tlinker.ld $< -nostdlib -static -o $@

%.dump: %.elf
	$(OBJDUMP) -d $< > $@

clean:
	rm -f *.elf *.dump *.img