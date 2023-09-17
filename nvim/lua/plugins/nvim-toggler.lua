return {
  'nguyenvukhang/nvim-toggler',
  config = function()
    require("nvim-toggler").setup({
      inverses = {
        ['vim'] = 'emacs',
        ['+'] = '-',
        ['<'] = '>',
      }
    })
  end
}
