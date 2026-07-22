-- Helper function to process options on an element
local function process_fold_or_hide(el)
  local fold_val = el.attributes['fold-or-hide']
  
  if fold_val ~= nil then
    if FORMAT == 'pdf' or FORMAT == 'latex' or FORMAT == 'typst' then
      -- In PDF/LaTeX/Typst: hide the code block entirely
      el.attributes['echo'] = 'false'
    elseif FORMAT == 'html' then
      -- HTML output: Set native Quarto code-fold attribute
      el.attributes['code-fold'] = fold_val
      el.attributes['fold-or-hide'] = nil -- Clean up custom attribute
    end
  end
  return el
end

return {
  -- Catches executable code cells (e.g. ```{r}, ```{python})
  Div = function(div)
    if div.classes:includes('cell') or div.attributes['fold-or-hide'] then
      return process_fold_or_hide(div)
    end
  end,
  
  -- Catches standard non-executable code blocks (e.g. ```r)
  CodeBlock = function(cb)
    return process_fold_or_hide(cb)
  end
}