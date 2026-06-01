# Renderers voor correcte weergave van valuta met euroteken, percentages en getallen.
renderer_nl_money <- JS(
  "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL', { style: 'currency', currency: 'EUR' }); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }"
)
renderer_nl_percent <- JS(
  "function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        var numVal = NaN;
        if (value !== null && value !== void 0 && value !== '') {
          var str = value.toString().replace(/\\./g, '').replace(',', '.');
          numVal = parseFloat(str);
        }
        if (!isNaN(numVal)) {
          if (numVal > 1) {
            td.innerHTML = numVal.toLocaleString('nl-NL', { style: 'currency', currency: 'EUR' });
          } else {
            td.innerHTML = numVal.toLocaleString('nl-NL', { style: 'percent', minimumFractionDigits: 2 });
          }
        }
        td.style.background = 'white';
        td.style.color = 'black';
        td.style.textAlign = 'right';
      }"
)
renderer_nl_general <- JS(
  "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); var numVal = NaN; if (value !== null && value !== void 0 && value !== '') { var str = value.toString().replace(/\\./g, '').replace(',', '.'); numVal = parseFloat(str); } if (!isNaN(numVal)) { td.innerHTML = numVal.toLocaleString('nl-NL'); } td.style.background = 'white'; td.style.color = 'black'; td.style.textAlign = 'right'; }"
)
