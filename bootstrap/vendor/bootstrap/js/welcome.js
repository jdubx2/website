


d3.select('#svg8')
  .attr('width', $(window).width() * .55)
  .attr('height', $(window).width() * .55 * .25);


let repeat = () => {
  var path = d3.selectAll('.svg_letter')
  .attr('fill', 'rgba(185,185,185,0)')
  // .attr('stroke', 'rgba(185,185,185,0)');
  .attr('stroke', '#2C7873');

  var totalLength = path.node().getTotalLength();

  path.attr("stroke-dasharray", totalLength + " " + totalLength)
    .attr("stroke-dashoffset", totalLength)
    .transition()
      .attr('stroke', '#2C7873')
      .duration(5000)
      .ease(d3.easeLinear)
      .attr("stroke-dashoffset", 0)
    .transition()
      .duration(2000)
      .attr('fill', 'rgba(185,185,185,1)')
      // .style('stroke-width', '.45px')
      .ease(d3.easeLinear)
      .transition()
      .delay(2000)
        .duration(2000)
        .attr('fill', 'rgba(185,185,185,0)')
        .attr('stroke', 'rgba(185,185,185,0)')
        .on("end", repeat);
  }
$(document).ready(repeat());
