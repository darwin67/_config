--!nonstrict
-- Month-to-date spend for the Summit Claude Console organization. The AI Usage
-- plugin reads one Console admin key, so this capsule runs ai-usagebar against
-- a second config file. Its own cache directory keeps the two organizations
-- from overwriting each other's cached spend and rate-limit backoff.
-- Drop this plugin once ai-usagebar supports [[anthropic_api.accounts]]
-- (https://github.com/akitaonrails/ai-usagebar/issues/599).

local COMMAND = 'XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}/ai-usagebar-console-summit" ai-usagebar'
    .. ' --config "${XDG_CONFIG_HOME:-$HOME/.config}/ai-usagebar/console-summit.toml" --vendor anthropic_api'
    .. " --format '{aapi_headline}'"
    .. " --tooltip-format 'Claude API · Summit: {aapi_headline}, through {aapi_through}'"

local inFlight = false

local function refresh()
    if inFlight then return end
    inFlight = noctalia.runAsync(COMMAND, function(result)
        inFlight = false
        local out = result ~= nil and not result.timedOut
            and noctalia.json.decode(result.stdout or "") or nil
        if type(out) == "table" and type(out.text) == "string" then
            -- ai-usagebar colors the text with Pango spans meant for Waybar.
            local text = out.text:gsub("<[^>]*>", ""):gsub("&amp;", "&")
            barWidget.setText(text)
            barWidget.setTooltip(out.tooltip)
        else
            barWidget.setText("—")
            barWidget.setTooltip("Claude API · Summit: ai-usagebar failed. "
                .. tostring(result and result.stderr or ""))
        end
    end, 30000)
end

function update()
    refresh()
end

function onClick()
    noctalia.runAsync("xdg-open https://console.anthropic.com/usage")
end

function onRightClick()
    refresh()
end

barWidget.setGlyph("asterisk-simple")
barWidget.setText("—")
noctalia.setUpdateInterval(5 * 60 * 1000)
refresh()
