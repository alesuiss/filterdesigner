![filterdesigner](/doc/screenshot.png?raw=true "filterdesigner")

About
=====

`filterdesigner` is a program inspired by the [Tone Stack Calculator](http://www.duncanamps.com/tsc/), except that you can design your own arbitrary schematic instead of using pre-defined "tone stacks". You define your schematic, including basic passive components, op-amps, and potentiometers; then you can simulate the circuit's frequency response, adjusting the potentiometer positions to see the impact in real time.

The intended use is to help design tone circuits for guitar amplifiers or other audio gear, but maybe it can have other uses.

You should consider this software to be alpha quality. The basic functionality works (rather well), but the UI lacks much polish, and bugs should be expected, especially if your circuits is "wrong" in some way: the program does absolutely no sanity check on it before attempting to simulate. Save your work before simulating :-)

Usage
=====

The program start with a blank circuit with just two "pseudo-components", the input on the left and the output on the right.
- select a tool on the tool bar (or using the hotkeys S, W, R, C, L, O, P, G)
- component tools: left click to place the component, right-click to rotate the tool by 90 degrees
- wire tool: draw a wire by clicking, holding and dragging
- selection tool: select a component to change its value
- you can drag-select multiple components to easily ajust all values after drawing the schematic
- the circuit currently needs to be **planar**; crossing wires are not allowed
- make sure you include a **ground**, or the simulator will not work
- your circuit need an AC path between input and output, and bewteen input/output and ground
- when everything is in place, use simulation / run in the menu to show the frequency response window

TODO
====

- UI features (zoom, pan, some polish, ...)
- it should be quite easy to display the phase response as well as frequency
- sanity checks on the circuit before simulating
- better docs :-)
