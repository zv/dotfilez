# ============================================================================
# FILE: matcher/project_files.py
# AUTHOR: Shougo Matsushita <Shougo.Matsu at gmail.com>
# License: MIT license
# ============================================================================

from denite.base.filter import Base
from denite.util import path2project, Nvim, UserContext, Candidates


class Filter(Base):

    def __init__(self, vim: Nvim) -> None:
        super().__init__(vim)

        self.name = 'matcher/project_files'
        self.description = 'project files matcher'

    def filter(self, context: UserContext) -> Candidates:
        project = path2project(self.vim,
                               context.get('path', ''),
                               context.get('root_markers', ''))
        if project == '':
            project = self.vim.call('getcwd')
        project += '/'

        max_width = int(context['max_candidate_width'])
        return [x for x in context['candidates']
                if 'action__path' not in x or
                x['action__path'][:max_width].startswith(project)]
