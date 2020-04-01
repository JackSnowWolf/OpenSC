class NotesController < ApplicationController
    def create
        @subject = Subject.find(params[:subject_id])
        puts "lol"
        # This will automatically link the note so that it belongs to that particular subject.
        new_params = {:user_id => session[:user_id]}
        @note = @subject.notes.create(new_params.merge(note_params))

        redirect_to subject_path(@subject)
    end

    def destroy
        @subject = Subject.find(params[:subject_id])
        @note = @subject.notes.find(params[:id])
        @note.destroy
        redirect_to subject_path(@subject)
    end

    private

        def note_params
            params.require(:note).permit(:user_id, :title, :body)
        end
end
