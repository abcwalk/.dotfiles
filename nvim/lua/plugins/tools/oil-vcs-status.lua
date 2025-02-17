local status_ok, oilvcs = pcall(require, 'oil-vcs-status')
if not status_ok then
    return
end

local status_const = require('oil-vcs-status.constant.status')
local StatusType = status_const.StatusType

oilvcs.setup({
    status_symbol = {
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

        [StatusType.UpstreamAdded] = 'A',
        [StatusType.UpstreamCopied] = 'C',
        [StatusType.UpstreamDeleted] = 'D',
        [StatusType.UpstreamIgnored] = '',
        [StatusType.UpstreamModified] = 'M',
        [StatusType.UpstreamRenamed] = 'R',

        [StatusType.UpstreamTypeChanged] = 'T',
        [StatusType.UpstreamUnmodified] = 'M',
        [StatusType.UpstreamUnmerged] = 'U',
        [StatusType.UpstreamUntracked] = '?',
        [StatusType.UpstreamExternal] = 'X',
        [StatusType.UpstreamExternal] = 'X',
    },
})
