return {
    'nvim-lualine/lualine.nvim',
    config = function()
        require('lualine').setup({
            options = {
                theme = 'auto',
                globalstatus = true,
                component_separators = { left = '', right = '' },
                section_separators = { left = '', right = '' },
                always_divide_middle = true,
                disabled_filetypes = {
                    statusline = { 'alpha', 'NvimTree', 'trouble', 'Outline' },
                },
            },
            sections = {
                lualine_a = {},
                lualine_b = {
                    {
                        function()
                            local function get_branch(cmd, error_patterns)
                                local handle = io.popen(cmd .. ' 2>/dev/null')
                                if not handle then
                                    return ''
                                end

                                local output = handle:read('*a')
                                local success, _, code = handle:close()

                                if not success or code ~= 0 then
                                    return ''
                                end

                                for _, pattern in ipairs(error_patterns) do
                                    if output:match(pattern) then
                                        return ''
                                    end
                                end

                                local clean_output = output:gsub('%s+', '')
                                return clean_output ~= '' and clean_output or ''
                            end

                            local git_branch = get_branch('git rev-parse --abbrev-ref HEAD', { 'fatal:', 'error:' })

                            if git_branch ~= '' then
                                return git_branch
                            end

                            return get_branch(
                                'ast branch --show-current',
                                { 'fatal:', 'error:', 'not a git repository' }
                            )
                        end,
                        icons_enabled = true,
                        icon = '',
                    },
                    {
                        'filename',
                        path = 4,
                    },
                },
                lualine_c = {
                    {
                        function()
                            local venv_path = require('venv-selector').venv()
                            local venv_name = venv_path:match('([^/]+)$')
                            if venv_name and venv_name ~= '' then
                                return string.format('[venv: %s]', venv_name)
                            end
                        end,
                    },
                },
                lualine_x = {},
                lualine_y = {
                    {
                        'location',
                    },
                    {
                        'lsp_status',
                    },
                },
                lualine_z = {},
            },
        })
    end,
}
