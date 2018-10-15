# R-dashboard-template

This is just a helpful (I hope) template to help build Shiny Dashboard apps. It's structured as a simple UI displaying dynamic UI elements generated in the server code. The code uses a built-in data set in R regarding crime rates in US states (USArrests), as well as map data in the ggplot2 library.

### Usage

The template contains examples on:
- A static, sortable/searchable data table
- An example of a dynamic, active data plot that responds to inputs (Go, Stop, Reset)
- Value and Info Boxes
- Geographic Mapping (with heatmap colors and filters)
- Example of dynamic, filterable data tables with checkbox and slider filters (and pop-up messages)

Since most of the rendering is done in the server code, some of the complications of reactivity are minimized.

You can use the template with few (if any alterations) to the UI code. Instead, the server code can be altered with different data sets and different parsing options.

If desired, however, there are "go-bys" in the UI code to enable formatting and interactive Javascript code.

### Contact: Jim Harris jimeharrisjr@gmail.com
