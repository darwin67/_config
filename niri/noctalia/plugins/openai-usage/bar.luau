--!nonstrict
-- Month-to-date spend for the OpenAI Platform organization. ai-usagebar reads
-- OpenAI usage only from Codex logins, so the openai-usage script (see
-- nixos/common/desktop/pkg.nix) sums the Admin API's daily costs instead.

local inFlight = false

local function refresh()
    if inFlight then return end
    inFlight = noctalia.runAsync("openai-usage", function(result)
        inFlight = false
        local out = result ~= nil and not result.timedOut
            and noctalia.json.decode(result.stdout or "") or nil
        if type(out) == "table" and type(out.text) == "string" then
            barWidget.setText(out.text)
            barWidget.setTooltip(out.tooltip)
        else
            barWidget.setText("—")
            barWidget.setTooltip("OpenAI API: openai-usage failed. "
                .. tostring(result and result.stderr or ""))
        end
    end, 30000)
end

function update()
    refresh()
end

function onClick()
    noctalia.runAsync("xdg-open https://platform.openai.com/usage")
end

function onRightClick()
    refresh()
end

barWidget.setGlyph("brand-openai")
barWidget.setText("—")
noctalia.setUpdateInterval(5 * 60 * 1000)
refresh()
