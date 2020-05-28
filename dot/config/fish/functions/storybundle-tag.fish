function storybundle-tag --description "Adds a StoryBundle tag to git annex metadata for newly-added not-yet-committed files."
    if test (count $argv) -ne 1
        echo "Usage: storybundle-tag 'Shameful House Bundle'"
        return 1
    end
    set tag $argv[1]

    if not set -l toplevel (git rev-parse --show-toplevel 2>/dev/null)
        echo "Please run from inside your Books annex git repository"
        return 1
    end

    if not test -d "$toplevel/StoryBundle"
        echo "This repository does not have a StoryBundle subdirectory. I am confused"
        return 1
    end

    begin
        pushd $toplevel
        git annex metadata -s "StoryBundle+=$tag" (git diff --name-only --cached StoryBundle/)
    end
    popd
end
