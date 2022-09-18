
# generate expected results for adler32 test
from zlib import adler32

print(adler32(b"Hello world!"))
print(adler32(b"The quick brown fox jumped over the lazy dog"))
print(adler32(b"the Quick bRown fOx jumPed oveR tHe lAzy doG"))
print(adler32(b"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ac libero eu orci efficitur ultricies."))
