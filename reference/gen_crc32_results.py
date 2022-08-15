
# generate expected results for crc32 test
from zlib import crc32

print(crc32(b"Hello world!"))
print(crc32(b"The quick brown fox jumped over the lazy dog"))
print(crc32(b"the Quick bRown fOx jumPed oveR tHe lAzy doG"))
print(crc32(b"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ac libero eu orci efficitur ultricies."))
