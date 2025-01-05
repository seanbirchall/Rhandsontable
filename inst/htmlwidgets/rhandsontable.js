HTMLWidgets.widget({
  name: 'rhandsontable',
  type: 'output',
  factory: function(el, width, height) {
    var hotElement = document.createElement('div');
    el.appendChild(hotElement);
    return {
      renderValue: function(x) {
        if (el.hot && el.hot.destroy) {
          el.hot.destroy();
        }
        const inputId = el.id + '_action';

        // Process menu items from R
        function processMenuItems(items) {
          const processedItems = {};
          if (!items) return processedItems;

          Object.entries(items).forEach(([key, item]) => {
            if (item.submenu) {
              processedItems[key] = {
                name: item.name,
                submenu: {
                  items: item.submenu.map(subItem => ({
                    key: subItem.key,
                    name: subItem.name,
                    callback: subItem.value ? function(key, options) {
                      Shiny.setInputValue(inputId, {
                        action: subItem.value,
                        id: new Date().getTime(),
                        column: this.getColHeader(this.getSelected()[0][1])
                      });
                    } : undefined
                  }))
                }
              };
            } else {
              processedItems[key] = {
                name: item.name,
                callback: item.value ? function(key, options) {
                  Shiny.setInputValue(inputId, {
                    action: item.value,
                    id: new Date().getTime(),
                    column: this.getColHeader(this.getSelected()[0][1])
                  });
                } : undefined
              };
            }
          });
          return processedItems;
        }

        // Cell configuration function
        function getCellConfig(row, col) {
          let config = {
            renderer: customRenderer
          };

          // Check cell-specific readonly status
          if (Array.isArray(x.readOnlyCells)) {
            x.readOnlyCells.forEach(cell => {
              if (cell && cell.row === row + 1 && cell.col === col + 1) {
                config.readOnly = cell.readonly;
              }
            });
          }

          // Check column readonly status
          if (Array.isArray(x.readOnlyCols)) {
            x.readOnlyCols.forEach(colConfig => {
              if (colConfig && colConfig.cols) {
                const shouldApply = typeof colConfig.cols === 'string'
                  ? x.colHeaders[col] === colConfig.cols
                  : colConfig.cols.includes(col + 1);

                if (shouldApply) {
                  config.readOnly = colConfig.readonly;
                }
              }
            });
          }

          // Check row readonly status
          if (Array.isArray(x.readOnlyRows)) {
            x.readOnlyRows.forEach(rowConfig => {
              if (rowConfig && Array.isArray(rowConfig.rows) &&
                  rowConfig.rows.includes(row + 1)) {
                config.readOnly = rowConfig.readonly;
              }
            });
          }

          return config;
        }

        // Custom cell renderer for styling
        function customRenderer(instance, td, row, col, prop, value, cellProperties) {
          // Call the original renderer
          Handsontable.renderers.TextRenderer.apply(this, arguments);

          try {
            // Apply cell-specific styles
            if (Array.isArray(x.cellStyles)) {
              x.cellStyles.forEach(style => {
                if (style && style.row === row + 1 && style.col === col + 1) {
                  Object.assign(td.style, style.style || {});
                }
              });
            }

            // Apply column styles
            if (Array.isArray(x.colStyles)) {
              x.colStyles.forEach(style => {
                if (style && style.cols) {
                  const shouldApplyStyle = typeof style.cols === 'string'
                    ? instance.getColHeader(col) === style.cols
                    : style.cols.includes(col + 1);

                  if (shouldApplyStyle) {
                    Object.assign(td.style, style.style || {});
                  }
                }
              });
            }

            // Apply row styles
            if (Array.isArray(x.rowStyles)) {
              x.rowStyles.forEach(style => {
                if (style && Array.isArray(style.rows) && style.rows.includes(row + 1)) {
                  Object.assign(td.style, style.style || {});
                }
              });
            }
          } catch (e) {
            console.error('Error applying styles:', e);
          }

          return td;
        }

        // Create new instance
        try {
          el.hot = new Handsontable(hotElement, {
            data: x.data,
            colHeaders: x.colHeaders,
            rowHeaders: x.rowHeaders,
            licenseKey: 'non-commercial-and-evaluation',
            height: '100%',
            width: '100%',
            manualColumnResize: true,
            manualRowResize: true,
            wordWrap: false,
            cells: function(row, col) {
              return getCellConfig(row, col);
            },
            dropdownMenu: x.menuItems ? {
              items: processMenuItems(x.menuItems)
            } : false,
            contextMenu: x.menuItems ? {
              items: processMenuItems(x.menuItems)
            } : false,
            afterInit: function() {
              this.selectCell(0, 0);
            },
            afterChange: function(changes, source) {
              if (!changes) return;
              Shiny.setInputValue(el.id, {
                data: this.getData(),
                changes: changes,
                params: this.params
              });
            },
            afterSelection: function(r, c, r2, c2) {
              if (!HTMLWidgets.shinyMode) return;
              Shiny.setInputValue(el.id + '_select', {
                r: r + 1,
                c: c + 1,
                r2: r2 + 1,
                c2: c2 + 1
              });
            }
          });
          el.hot.params = x;

          if (HTMLWidgets.shinyMode) {
            Shiny.setInputValue(el.id, {
              data: el.hot.getData(),
              params: el.hot.params
            });
          }
        } catch (e) {
          console.error("Error creating Handsontable instance:", e);
        }
      },
      resize: function(width, height) {
        if (el.hot && el.hot.updateSettings) {
          el.hot.updateSettings({
            width: width,
            height: height
          });
        }
      }
    };
  }
});
