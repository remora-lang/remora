#!/usr/bin/env python3
# usage: thisScript --weights yolov4.weights --image dog.jpg --output data/yolov4.in
# writes the entry input of yolov4.remora in the Futhark binary data format:
# the 3x608x608 image tensor followed by the flat 64429405-element weight blob
import argparse
import struct
import sys

SIDE = 608
WEIGHTS = 64429405


def put(out, dims, floats):
    out.write(b'b\x02' + bytes([len(dims)]) + b' f32')
    for d in dims:
        out.write(struct.pack('<Q', d))
    out.write(floats)


def read_weights(path):
    with open(path, 'rb') as f:
        major, minor, _revision = struct.unpack('<iii', f.read(12))
        f.read(8 if major * 10 + minor >= 2 else 4)
        blob = f.read()
    if len(blob) != WEIGHTS * 4:
        sys.exit(f'{path}: expected {WEIGHTS} weights after the header, got {len(blob) / 4:.0f}')
    return blob


def read_image(path):
    if path.endswith('.bin'):
        with open(path, 'rb') as f:
            blob = f.read()
        if len(blob) != 3 * SIDE * SIDE * 4:
            sys.exit(f'{path}: expected {3 * SIDE * SIDE} floats, got {len(blob) / 4:.0f}')
        return blob
    try:
        from PIL import Image
    except ImportError:
        sys.exit('reading %s needs Pillow; pass a raw .bin of %d float32 instead'
                 % (path, 3 * SIDE * SIDE))
    image = Image.open(path).convert('RGB').resize((SIDE, SIDE), Image.BILINEAR)
    pixels = image.tobytes()
    planes = bytearray(3 * SIDE * SIDE * 4)
    for channel in range(3):
        struct.pack_into('<%df' % (SIDE * SIDE), planes, channel * SIDE * SIDE * 4,
                         *[pixels[i * 3 + channel] / 255.0 for i in range(SIDE * SIDE)])
    return bytes(planes)


def main() -> int:
    parser = argparse.ArgumentParser(
        prog='make_input',
        description='builds the yolov4.remora entry input from a darknet checkpoint and an image')
    parser.add_argument('--weights', required=True, help='darknet yolov4.weights')
    parser.add_argument('--image', required=True, help='image file, or a raw .bin of 3*608*608 float32')
    parser.add_argument('--output', default='data/yolov4.in', help='where to write the dataset')
    args = parser.parse_args()

    image = read_image(args.image)
    weights = read_weights(args.weights)
    with open(args.output, 'wb') as out:
        put(out, [3, SIDE, SIDE], image)
        put(out, [WEIGHTS], weights)
    return 0


if __name__ == '__main__':
    sys.exit(main())
