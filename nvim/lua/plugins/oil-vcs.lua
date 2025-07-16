return {
    'SirZenith/oil-vcs-status',
    config = function()
        local status_const = require('oil-vcs-status.constant.status')
        local StatusType = status_const.StatusType

        local status_symbol = {
            [StatusType.Added] = 'A',
            [StatusType.Copied] = 'C',
            [StatusType.Deleted] = 'D',
            [StatusType.Ignored] = '',
            [StatusType.Modified] = 'M',
            [StatusType.Renamed] = 'R',
            [StatusType.TypeChanged] = 'T',
            [StatusType.Unmodified] = 'M',
            [StatusType.Unmerged] = 'U',
            [StatusType.Untracked] = '?',
            [StatusType.External] = 'X',
        }

        for k, v in pairs(status_symbol) do
            local upstream_key = 'Upstream' .. k
            if StatusType[upstream_key] then
                status_symbol[StatusType[upstream_key]] = v
            end
        end

        require('oil-vcs-status').setup({ status_symbol = status_symbol })

        -- Disable background color for status symbols
        local highlights = {
            'OilVcsStatusUntracked',
            'OilVcsStatusAdded',
            'OilVcsStatusCopied',
            'OilVcsStatusDeleted',
            'OilVcsStatusIgnored',
            'OilVcsStatusModified',
            'OilVcsStatusUnModified',
            'OilVcsStatusRenamed',
            'OilVcsStatusUpstreamAdded',
            'OilVcsStatusUpstreamCopied',
            'OilVcsStatusUpstreamDeleted',
            'OilVcsStatusUpstreamIgnored',
            'OilVcsStatusUpstreamModified',
            'OilVcsStatusUpstreamUnModified',
            'OilVcsStatusUpstreamRenamed',
            'OilVcsStatusUpstreamUntracked',
        }

        for _, highlight in ipairs(highlights) do
            vim.api.nvim_set_hl(0, highlight, { bg = 'none' })
        end
    end,
}
