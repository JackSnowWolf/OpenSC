class SubjectsController < ApplicationController
    def new
        @subject = Subject.new
    end
    
    def create 
        # now the form is saved into this instance variable @subject in memory
        @subject = Subject.new(subject_params)

        # validation is defined in 'Subject' model
        if @subject.save
            # redirect to 'show' action
            redirect_to @subject
        else
            # 'render' is done within the same request
            # 'redirect_to' will tell the browser to issue another request.
            render 'new'
        end
    end

    def index
        @subjects = Subject.all
    end

    def show
        @subject = Subject.find(params[:id])
    end

    def edit
        @subject = Subject.find(params[:id])
    end

    def update
        @subject = Subject.find(params[:id])

        if @subject.update(subject_params)
            redirect_to @subject
        else
            render 'edit'
        end
    end

    def destroy
        @subject = Subject.find(params[:id])
        # 'destroy' is a callback: the in-memory object is still readable
        @subject.destroy

        # no need a view for this action, because it's redirected
        redirect_to subjects_path
    end

    private
        def subject_params
            params.require(:subject).permit(:title, :description)
        end
end
