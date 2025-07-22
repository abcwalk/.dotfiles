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
                            local handle = io.popen('git rev-parse --abbrev-ref HEAD 2>&1')
                            if handle then
                                local output = handle:read('*a')
                                handle:close()
                                if output:match('fatal:') then
                                    local ast_handle = io.popen('ast branch --show-current 2>&1')
                                    if ast_handle then
                                        local ast_output = ast_handle:read('*a')
                                        ast_handle:close()
                                        if
                                            ast_output:match('error: no repository')
                                            or ast_output:match('not a git repository')
                                        then
                                            return ''
                                        else
                                            return ast_output:gsub('%s+', '')
                                        end
                                    else
                                        return ''
                                    end
                                else
                                    return output:gsub('%s+', '')
                                end
                            else
                                return ''
                            end
                        end,
                        icons_enabled = true,
                        icon = '',
                    },
                },
                lualine_c = {
                    {
                        'filename',
                        path = 4,
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
                    {
                        function()
                            local venv_path = require('venv-selector').venv()
                            local venv_name = venv_path:match('([^/]+)$')
                            return '(' .. venv_name .. ')'
                        end,
                    },
                },
                lualine_z = {},
            },
        })
    end,
}
