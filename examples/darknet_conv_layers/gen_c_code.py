#!/usr/bin/env python3
# usage: thisScript yolov4_layers.txt
# will print C code to stdout
import argparse
import re
import sys
    
def main() -> int:
    parser = argparse.ArgumentParser(
        prog='gen_c_code',
        usage="%(prog)s inputFilePath",
        description='generates to stdout C code that contains concrete calls to polymorphic conv2d')
    parser.add_argument('inFilePath', help="path to input file",
                        action='store', nargs='?',
                        default='yolov4_layers.txt')
    args = parser.parse_args()

    with open(args.inFilePath, 'r') as f:
        print("switch(layer.index) {")
        _ignore = f.readline() # ignore 1st line
        for line in f:
            fields = re.split(r'[\sx/]+', line)
            #    layer   filters  size/strd(dil)      input                output
            #    0 conv     32       3 x 3/ 1    608 x 608 x   3 ->  608 x 608 x  32 0.639 BF
            #  0 1  2       3        4   5  6     7    8       9 10  11     12    13   14 
            # print("0:", fields[0], "1:", fields[1], "2:", fields[2], "3:", fields[3], "4:", fields[4],
            #       "5:", fields[5], "6:", fields[6], "7:", fields[7], "8:", fields[8], "9:", fields[9],
            #       "10:", fields[10], "11:", fields[11], "12:", fields[12], "13:", fields[13],
            #       "14:", fields[14])
            layer = fields[1]
            print(f"  case {layer}: conv2d/layer_{layer}(im, w); break;")
        print('  default: printf("ERROR: no matching conv2d function for layer %d", layer.index);')
        print('};');
    return 0

if __name__ == '__main__':
    sys.exit(main())
