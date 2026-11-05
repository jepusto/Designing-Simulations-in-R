-- Divs with these classes become matching LaTeX environments in PDF output
-- (in HTML they just stay as <div class="..."> for CSS to style)
local envs = { epigraph = true }

return {
  Div = function(div)
    if FORMAT == 'pdf' or FORMAT == 'latex' then
      for class, _ in pairs(envs) do
        if div.classes:includes(class) then
          local blocks = pandoc.List()
          blocks:insert(pandoc.RawBlock('latex', '\\begin{' .. class .. '}'))
          for _, b in ipairs(div.content) do
            blocks:insert(b)
          end
          blocks:insert(pandoc.RawBlock('latex', '\\end{' .. class .. '}'))
          return blocks
        end
      end
    end
  end
}
