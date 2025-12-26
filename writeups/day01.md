# SPOILERS!!!
This writeup assumes that you've already read AND solved day 1. If you haven't you'll probably get spoiled.

### Why FPGA?
Since you're already reading this, I presume that you *want* to learn more about FPGAs and more importantly already know a little about them. In case you don't, I'd advise you to read up on them online. [IBM's page](https://www.ibm.com/think/topics/field-programmable-gate-arrays) is nice.

Now, to restate the problem without the fluff. There's a circular dial with 100 positions (0-99) that starts at position 50, and we need to compute:

- **Part 1**: Count of movements that END at position 0
- **Part 2**: Count of individual ticks (steps) where the position BECOMES 0


### Input Encoding

We can encode each movement encoded as a 16-bit word where the high bit encodes the direction.

Encoding logic (`encode_input`):
```ocaml
let dir_bit = if Char.equal dir 'L' then 0x8000 else 0 in
dir_bit lor dist
```

### Solver State Machine

The actual solver (`day01_solver.ml`) implements a 6-state Finite State Machine (FSM). If you don't know what a FSM is (or forgot), here're some good [lecture slides](https://web.stanford.edu/class/cs123/lectures/CS123_lec07_Finite_State_Machine.pdf) from Stanford which I thought were super helpful and concise.

TODO: Make diagram

Our FSM will have 6 different states:

- **Idle**: Initializes registers (position=50, counters=0), waits for `start` signal
- **Reading**: Checks if `read_addr >= input_count`; if not, proceeds to fetch next input
- **Wait_ram**: 1-cycle RAM latency; decodes direction (bit 15) and distance (bits 14:0)
- **Ticking**: Executes one tick per cycle:
   - Updates position with circular wrap (0 <-> 99)
   - Increments `part2_count` if next position is 0
   - Decrements `tick_counter`
- **Check_end**: At end of movement:
   - Increments `part1_count` if final position is 0
   - Advances `read_addr` to next input
- **Done**: Asserts `done_` signal, outputs are valid

## Registers

For the uninitiated, you can think of registers as physical 'variables' that can hold values, with a few important distinctions [^1]. The one distinction you really need to know is that register assignments describe what happens on the *next* clock edge, not immediately. We will use 6 registers:
- `read_addr` (13-bit): This register will be used to traverse the input. My input has 4577 lines, hence the width is 13 bits since ceil(log_2 4577) = 13.
- `position` (7-bit): This register will store the actual dial position. Again ceil(log_2 99) = 7 hence 7 bits
- `direction` (1-bit): convenient register to store direction
- `tick_counter` (10-bit): register to store how many ticks left in our current input. [^2]
- `part1_count` (32-bit): register to store final result
- `part2_count` (32-bit): register to store final result

### Hardcaml Basics
Hardcaml is an OCaml library that generates hardware. Your OCaml runs once to produce a circuit, which then gets synthesized onto the FPGA. [^3]
Quick reference for the operators you'll see:

- `<--` / `<--.` : assign signal / constant to register
- `==:` / `==:.` : compare signals / compare to constant
- `mux2 sel a b` : if `sel` then `a` else `b`
- `when_ cond [...]` : conditional execution
- `.value` : read a register's current value

Now, for the one tricky bit for the day, circular logic:

```ocaml
let next_pos_left =
  mux2 (position.value ==:. 0) (of_int_trunc ~width:7 99) (position.value -:. 1)
in
let next_pos_right =
  mux2 (position.value ==:. 99) (of_int_trunc ~width:7 0) (position.value +:. 1)
in
let next_pos = mux2 (direction.value ==:. 1) next_pos_left next_pos_right
```

- **Left**: position 0 wraps to 99, otherwise decrements
- **Right**: position 99 wraps to 0, otherwise increments

[^1]: Unlike software variables, registers only update on clock edges (not immediately), and they're physical resources that consume silicon. See [Nandland's explanation](https://nandland.com/project-3-the-flip-flop-aka-register/) for more.

[^2]: In my input the max distance was <=999, hence 10 bits

[^3]: Think of it like React for hardware: you write declarative descriptions, and the framework figures out the actual wiring.