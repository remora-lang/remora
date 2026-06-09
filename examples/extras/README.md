# Remora Examples

The files in this directory are intended to be used as examples to help learn
the Remora language. As of this writing, there is a **huge** discrepancy between
the Remora language description and what is available in the implementation.
Part of our motivation is to make this distinction clear, help new Remora
programmers to get started with the language, help Remora implementors
to prioritize implementation effort, and ask some questions about the language.

## Example Descriptions and Lessons Learned from Each

### row-col-sums

This example explores Remora's @reduce function. To start with, it should be
noted that the documentation talks about the reduce function, without a leading
@ sign. That is a somewhat minor difference with the implementation. A more
important difference is that when @reduct is applied to matrices, it somewhat
naturally produces column sums (though a custom function that performs plus on
vectors is required, as Remora does not lift the binary + automatically).
But there seems to be no way to create row sums. The documentation suggests that
this is possible by building the right type signature for an "add" function,
but this does not appear to be the case. Looking at some other Remora examples,
it appears that what is necessary is to take the transpose of the matrix, so
that is what this example shows.

### matmult

This example shows one approach to matrix multiplication in Remora. The technical
challenge here is that dot products of a row against a column are required, and
this stresses the polymorphism rules of the current implementation. In order to
get around that, we do the same thing we did in row-col-sums and transpose the
second matrix so that it lines up with the rows of the first matrix.

### convolution-1d

This example shows how to do a simple convolution in Remora, e.g., to smooth a
signal by averaging a value with its neighbors. The example is taken directly
from the Remora Tutorial, but there are many caveats. The Tutorial relies
heavily on the (rotate ...) built-in function, but this function does not
actually exist in the current implementation. We wrote our own, which serves
to show how powerful Remora is while exposing many limitations:

* The rotate function is implemented by using index operations.
* The Tutorial describes an (index ...) function, but it is not implemented.
* There is an (index2d ...) function that **is** implemented, and that's what
  we used, though it means the array and indexes have to be reshaped in odd
  ways.
* The (rotate ...) function does a circular shift, but this is complicated
  because Remora lacks modular arithmetic and simple conditional code, e.g.,
  to convert index 12 into 2 for a 10-element array. We got around this
  problem by allowing indexes to become larger than the array size, and
  duplicating the array so that the behavior of those large indexes is as
  expected.
* The index2d function does show a very nice feature of Remora's polymorphism,
  because the index can be replaced with an arbitrarily shaped array, and the
  result is an isomorphic array of values. That is exactly the behavior that is
  needed to implement rotate with index.

A limitation of this example (also present in the Tutorial) is that each
value is combined with the two values immediately after it. It does not, for
example, support combining a value with its neighbors, which would be more
useful in signal processing applications. That can be easily resolved, of
course, e.g., if the (technically non-existent) built-in rotate function can
rotate left or right. It would be even better if the rotate function could be
used in a non-circular mode, perhaps with an extra argument providing the
constant that is shifted in.

Another limitation of the current Remora implementation is apparent from this
example. In the original Tutorial, the cells in the array are integers. The
result of the convolution is the sum of each value and the two cells to the
right. But what about the average? I thought to make the weights floating-point,
so that we could implement a smoothing function, e.g., with weights
[0.25, 0.5, 0.25]. But this runs into two problems. First, as observed above
the convolution is mixing each value with the next two values, not with its two
neighbors. That could be fixed. More serious is that the arithmetic operations
for integer and floating-point are different, e.g., + for integers and f.+ for
floating points. This is surprising, since the implementation
allows/encourages/requires programmers to be explicit about types.

### rule30

This is an implementation of Stephen Wolfram's Rule30 cellular automaton. The
rule derives its name from the fact that it considers the eight possible
patterns created by a cell and its neighbors, and determines the next value of
the center cell from each pattern. The binary number resulting from the eight
possibilities adds up to 30.

This example demonstrates how to address the issue of convolution with
neighboring cells, not just to one side of the current cell. It also shows
how to do a "lookup" into a table, in order to implement the Rule30
semantics.

This example also shows a couple of warts:

* The solution to the convolution-with-neighbors problem is to rotate the
  cells to the right before doing the convolution, but rotating to the left
  (using the implementation) results in negative indexes. Rotating to the
  right results in indexes that are too large, but that can be paved over by
  duplicating the original array. No such solution appears possible for
  negative indexes, so we implemented rotate-right by using rotate-left
  the appropriate number of times. E.g., to rotate-right a 5-element array,
  we rotate-left by 4.
* The problem is exacerbated because to perform the rotation we need to know
  the size of the array. There is a built-in "length" function, but we were
  unable to figure out how to use it correctly. So we wrote our own by creating
  an isomorphic list of ones (taking advantage of Remora's built-in lifting of
  functions via an implicit map) and then using @reduce to tally the number.
  Needless to say, this is wildly inefficient.

### life

This is an implementation of Conway's Game of Life. The game is a 2-D cellular
automata, so this is a nice follow-up to Rule30. It also takes up the challenge
from the Tutorial to use Remora to implement a 2D convolution.

The rules of the Game of Life make use of the number of living neighbors of a
given cell. Each cell has eight neighbors so it can have between zero and eight
living neighbors. One way to capture this number is to shift the array up/down
and left/right and add the overlaid cells. Indeed, this is the strategy used
in the "darknet" example that is included in the Remora GitHub repository,
where a key function extracts a single column from a 2D array.

However, we chose to pick a different approach. We created an array of 2D
indexes for the board, and we shifted *that* array. More precisely, we created
a 1D array of indexes into a single row/column, shifted that 1D array, then
took the eight relevant cross products (which is an interesting application
of Remora's polymorphism).

In order to implement the Game of Life rules, we used the eight arrays of
indexes to index into the game board eight times, then we added up the eight
resulting (shifted) boards. In this way, we discovered a probably intensional
limitation of Remora, namely that the + function is strictly binary. Coming
from a LISP background, we found this surprising and unwelcome, but we realize
it may make sense when thinking about GPU processing.

Once the number of living neighbors is found, it is almost trivial to apply
the Game of Life rule. We used a lookup table to determine whether the cell
should live or die at the next generation, just as with Rule30. However, the
Game of Life has separate rules for living and dead cells, so we needed two
lookup tables. Where the challenge comes in is that Remora does not appear to
have conditional statements or conditional expressions, so how can we select
the appropriate rule based on the life status of a cell? We solved this problem
by exploiting the fact that the life status is stored as a single bit, and that
we can use arithmetic to reduce

    if X then Y else Z

into

    X*Y + (1-X)*Z

What can we say? Once a bittwiddler, always a bittwiddler.

### fft-fixed

This example demonstrates the use of Remora to implement the FFT. The FFT
strikes us as the kind of algorithm that Remora is designed to do.

The obvious challenge is that Remora does not support either loops or recursion.
Nor does it support complex numbers, though they can be represented as pairs
of reals. Remora's polymorphism immediately supports addition and subtraction
of complex numbers in this form, but we had to write our own implementation of
complex multiplication. One approach to do this is to use array indexing to
extract the real and imaginary components of a complex number. As an exercise,
we chose to do this using the built-in polymorphic lifting of binary functions
to arrays, together with @reverse to handle the cross-terms.

A serious problem is that Remora does not appear to have trigonometric or
exponential functions. It is worth noting that these will be required if
Remora is to be used in machine language applications, as they are required
to implement the softmax function, the sigmoid function, and other popular
activation functions. Of course, we needed the powers of the complex roots
of unity to implement the FFT, so we wrote a Python program that generates
an array of them in Remora syntax.

But this array is necessarily finite! As it turns out, that is not a major
limitation, since Remora does not support recursion or loops. SO the FFT has
to be unwound and written in purely combinational form. This is also what
happens in practice, e.g., when the FFT is implemented in hardware, so this is
likely an intensional limitation. The way this looks to a programmer is that
instead of writing one FFT function, she has to write a different FFT function
for each vector size. For example, we built fft1, fft2, fft4, fft8, and fft16,
and we could have extended this sequence to higher powers of two. What would be
really nice is if Remora could provide syntactic sugar to make this unfolding
automatic.

## Remaining Questions

* How does polymorphism work with +? If we need to use + or f.+, then the type
  variable &t in @reduce (for example) does not appear useful.
* Can we do conditionals?
* At the very least, max/min?
* Modular arithmetic would be great.
* Where did rotate go?
* Is there a 1D equivalent of @index2d?
* How does length work?
* Any trig functions? And exp?
* How exactly do i-app and t-app work, and when/why are they needed?
* Is there a naming convention for @functions? It seems they all take type
  and size parameters.
* Can we use the size parameters as variables or vice versa? Sometimes the
  code wants to know the size of the array being processed.
