# Testbench for Programming Test

## How to write and submit code

Time: 1 hour 30 minutes (approx)

Conditions: You must work in 305. You may use your own laptops, or dept workstations. You may use internet resources but not receive help from people.

Submission: You must submit your working code as a single `answer.fs` file (not fsx) to [HLP Programming Test](https://intranet.ee.ic.ac.uk/scripts2/UploadCW.aspx) before the specified deadline.

Testbench. You are given a testbench for `answers.fs` that runs under Visual Studio. You may use this as you are developing code.

The test comprises a set of related programming problems, the answer to each of which you write as a field of a record value `theAnswer: TestResults`. This record is tested by the testbench scoring you for any cases you have implemented. Unimplemented cases are set to a failing implementation by default and score zero marks.

1. For your convenience each function you need to submit has header already written in `answers.fs` with type information and a default failing implemnetation. Replace the implementation by your code. The precise specification for each function is given in comments above the header.

2. The predefined types *and values* that you must use are already written - don't write these again! The types and values are shown in a block comment at the start of `answers.fs`.

3. You are expected to add your own helper functions to `answers.fs` as needed to implement the required functions. A good solution will use functional abstraction and define functions in addition to those required.

4. The functions you are required to write are given in order. in many cases previous functions will be useful in the implementation of subsequent ones. therefore you should attempt functions in order.

5. YOU MAY NOT USE .NET libraries `DateTime` or `DateTimeOrder` to implement these functions!

6. The testbench may be modified. `testbench.fs` lines 104-119 contains the tests. You can comment tests out if you wish not to see failing tests for those questions you have not answered. the final question (Q7) has two tests. The last test is by default commented out since it fails very noisily. Uncomment this for aditional testing of your answer to Q7.

The testbench can be downloaded via [this link](images/testbenchreal.zip) at the start of the test.








