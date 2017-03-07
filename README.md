# ICFP Contest 2013 - Team "A Storm of Minds"

Instead of the usual writeup, I'll present our answers to the judges survey:

### 1. Did you do any preparation for the contest before the start date?

> Did you learn to use some new software packages?

No


> Did you make use of our hints prior to the contest?
>> Did you read any research papers on program synthesis?

I was content just to find out what it was about and watched one video about Excel 2013 autofill

>> Did you write any JSON or S-expression utilities?

No – dropping misleading hints is a well-established practice among ICFP contest organizers. Also, both of these formats have simplicity as a major feature and writing a parser is a matter of minutes.

### 2. After you read the problem description, what were all the strategies you considered using?

   1. Brute force – always a favorite. Generate lots/all programs until you find the solution.
   2. Smart: Start with any program and iteratively refine it with new Eval or Guess results.

### 3. Which strategy did you end up choosing and why?

Used the brute force approach since I was too tired to really think hard about how to construct programs satisfying certain arguments/results. Also, partially because of an initial misunderstanding of the rules: We assumed that our programs had to be of the exact size and use exactly the operators given for each problem. This wrong assumption seemed to make a “generate all” Approach more feasible and a “smart” approach somewhat less feasible.

Basically, we generate all possible programs before starting. This has the nice effect that the time spent generating possible programs is not relevant for the 300 seconds limit.

We then iteratively filter the programs with values from eval/guess until we win (or not). We always started with eval on 256 random numbers. This was supposed to be replaced by at least some smart number choices, like 0, 1, not 0 and powers of two, but somehow this plan got lost during the contest. All generated programs were evaluated with the first result, dropping all those not fitting. After that, the next value was tried, and so on. When running out of values, a new eval was issued. Whenever trying a value did not reduce the number of still possible programs, a guess with one of them was made. In case of mismatch, the provided example was used as next value to rule out problems.

> What was your strategy for the /eval requests?

Wanted to use a combination of fixed values (0, all single-bit values, single-byte values) plus some random values. Due to a communication error in our team, we ended up with random only.

> Once you received some set of input/output pairs for a program, what did your program do?

Remove all programs from the list which fail to produce the correct result for some input

> How did you make use of the counter-examples that we provided for each guess?

Same way, they were used with priority over the values received via eval.

### 4. What steps did you take to limit the search space of programs?

> For example, how did you exploit the following features:
>> Knowledge of the operators in the program

We used only the operators listed for each problem

>> Knowledge of the size of the program

Used in several places to limit the size of generated subexpressions

>> Commutative operators, e.g., `(and x y) = (and y x)`

We generated only programs where |x| <= |y|

>> Inverse operators,     e.g., `(not (not x)) = x`

Yes, filtered this out

>> Identity operators,    e.g., `(or x 0) = x`

Yes, and see below

>> Redundant operators,   e.g., `(if0 0 x y) = x`

Yes, and some more, e.g. `(if0 x y y) = y`

>> Larger identities,     e.g., `(shr1 (shr1 (shr1 (shr1 x)))) = (shr4 x)`

Only when shr4 was “available” due to our misunderstanding of the problems mentioned above

>> The iterative structure of fold

No, though we did some optimizations

>> The tfold hint

No – I don’t think we understood the hint, only used the knowledge that tfold rules out other folds and essentially reduces the size of the problem.

> Aside from this, what were the three most important steps you took  to limit the search space?

1. Using only the operators given in the problem set.
2. Omitting equivalent programs by exploiting commutativity, and things like `(shr4 (shr1 x)) = (shr1 (shr4 x))`
3. Doing a full value propagation pass and annotating all expressions with the range of possible values. Helps quite a lot. The exact formulas for result ranges of unsigned binary operations from Hank Warren’s wonderful book “Hacker’s Delight” helped quite a bit here.


### 5. What steps did you take to parallelize the search?

We figured early on that even a factor-10 speedup through parallelism would only enable us to solve problems of 1-2 size larger, so we did not concentrate on that approach. That said, in the later “kamikaze” mode, we ran our program on 5 systems.

> How much parallelism did you manage to achieve? For example, how
> many threads/processes did you have going in parallel?

5

> How did you manage the concurrency? Language support, libraries,
> OS-level process support?

OS-level, (different PCs, actually, as even our single-threaded program generator was memory-contrained).

> Did you solve any of the contest problems in parallel? If so, what
> was your strategy for doing that?

No

### Did you use different strategies for each class of problem?

> The classes of problems were:
>
> - small fold-free problems
> - large fold-free problems
> - tfold problems
> - fold problems
> - bonus problem set 1 (size 42)
> - bonus problem set 2 (size 137)

We wrote a special generator for the bonus problems, exploiting their structure to reduce the search space somewhat. Did not really help, as most were still too large. When sorting problems or setting the max size for problems we were rating save to try on real data, we treated tfold problems as being of less size.

### 7. How did you cope with the following elements of the game:

> Did you use the training mode a lot before attempting the contest problems?

Yes, actually for the first two days we only attempted problems of size and structure where training had shown we could reliably solve them. On the last day, tests were performed on real data, cause we were more likely running out of time than running out of problems.

> How did you deal with the element of risk in the game? For example, many of the large fold-free problems could in fact be solved with a small guess.

We did not risk burning problems early on, but on Sunday started setting kamikaze-solver on the big ones (above lvl 25) while trying to get as far as possible bottom-up with less risky solvers at the same time. We were adjusting the burning rate vs. speed ratio based on time and #problems left.

### 8. How many lines of code did you write, and in which languages?

About 850 lines of Haskell and 850 lines of Java.

### 9. How was that code structured?

> You could indicate the number of modules, the structure of their interfaces, and, if you wish, you
> could even include a picture of the module dependences.

Program generator:

    Program.hs   - ADT für \BV, printing
    Parser.hs    – parsing programs
    Eval.hs      – Evaluating programs and expressions
    Reduce.hs    – simplify programs
    Reduce2.hs   – simply using value range analysis
    Generator.hs – Generate all possible programs (using lazy list approach)
    Bonus.hs     – special generator for bonus problems
    Main.hs      – guess what

Solver:

    GetTrainingSet.java    - getting training data
    ConvertMyProblems.java - formatting problems for program input, generating stats
    ServerConnector.java   - calling the API, counting time for rounds and between requests
    Value.java             - a pair of input value and expected result
    Valuestorage.java      - holding the values yet to check, requesting new eval if needed
    Program.java           - just holding the program string
    Evaluator.java         - using a given program on a given value
    Guess.java             - main class, trying to solve the problem
    SolveMyProblems.java   - calling the solver in a loop on a set of problems


### 10. What kind of bugs did you run into when developing your solution? How did you discover and fix those bugs?

Chris: Bugs? I don’t think we had any bugs in our programs.

Jan: We only had hardware bugs (out of memory...)

### 11. What hardware did you use? e.g., laptop, university servers, cloud etc.

Laptop, normal PC, some other PCs in the office. We were somewhat hampered by having only 8 GB of RAM on our systems. Did run across the idea of using our co-workers machines for some parallelism a bit too late to make a difference.

### 12. Anything else that you would like to add?

Surprised that we did so well using only a brute-force approach. I’ve had fun implementing  the value-propagation pass (Reduce2.hs), and was delighted that the formulas from “Hacker’s Delight” actually helped.
 
Being able to use the usual `(take n (“lazy list of all possible programs”))` approach in Haskell was also helpful. Generating possible programs before actually starting the timer by eval seems like one of the really good ideas. When the deadline came near, it got very exciting to try and work on all problems in time without getting the burn ratio too high.

Seeing some rough scoreboard estimates really motivated us. Chatting in IRC was very nice, too.

-----

**2013-08-05** - Got mail:

> I'm happy to inform you that your team ranked #25 in the main division of this year's ICFP contest.

:-)
