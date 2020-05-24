# ============================================================================
# FILE: output.py
# License: MIT license
# ============================================================================

import re
import typing

from denite.base.source import Base
from denite.util import Nvim, UserContext, Candidates


class Source(Base):

    def __init__(self, vim: Nvim) -> None:
        super().__init__(vim)
        self.name = 'output'
        self.default_action = 'yank'
        self.kind = 'word'

    def define_syntax(self) -> None:
        if not self.context['args']:
            return
        cmd = self.context['args'][0]
        if re.fullmatch(r'hi(ghlight)?(!)?', cmd):
            self.define_syntax_for_highlight(cmd)

    def gather_candidates(self, context: UserContext) -> Candidates:
        args = context['args']

        if not args:
            return []

        first = args[0]
        output: typing.List[str] = []
        if first[0] != '!':
            cmdline = ' '.join(args)
            output = self.vim.call('execute', cmdline).splitlines()[1:]
        else:
            cmdline = ' '.join([first[1:]] + args[1:])
            output = self.vim.call('system', cmdline).splitlines()
        return [{'word': x} for x in output]

    def define_syntax_for_highlight(self, cmd: str) -> None:
        self.vim.command('syntax include syntax/vim.vim')
        hi_list = self.vim.call('execute', cmd).splitlines()[1:]
        for hi in (h.split()[0] for h in hi_list):
            syn_hi_name = (
                'syntax match vimHiGroup' +
                ' /' + hi + r'\>/' +
                ' nextgroup=' + hi +
                ' skipwhite'
            )
            syn_hi_xxx = (
                'syntax match ' + hi +
                ' /xxx/' +
                ' contained' +
                ' nextgroup=@vimHighlightCluster' +
                ' skipwhite'
            )
            self.vim.command(syn_hi_name)
            self.vim.command(syn_hi_xxx)
